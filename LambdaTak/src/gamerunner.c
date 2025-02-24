#include <libwebsockets.h>
#include <jansson.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

#define SERVER_ADDRESS "127.0.0.1"
#define PORT 9160
#define PROTOCOL "ws"

static int interrupted;
static struct lws *web_socket;
static char game_id[64] = "game6"; // Default game ID
static int move_number = 0;

struct session_data {
    char buffer[4096];
    int buffer_len;
};

char *create_connection_message() {
    json_t *root = json_object();
    json_object_set_new(root, "gameId", json_string(game_id));
    return json_dumps(root, JSON_COMPACT);
}

char *create_move_message(const char *notation, const char *color) {
    json_t *root = json_object();
    json_object_set_new(root, "moveGameId", json_string(game_id));
    json_object_set_new(root, "moveNotation", json_string(notation));
    json_object_set_new(root, "moveColor", json_string(color));
    return json_dumps(root, JSON_COMPACT);
}

void parse_game_response(const char *json_str) {
    json_error_t error;
    json_t *root = json_loads(json_str, 0, &error);
    
    if (!root) {
        fprintf(stderr, "JSON error: %s\n", error.text);
        return;
    }

    json_t *status = json_object_get(root, "responseStatus");
    json_t *msg = json_object_get(root, "message");
    json_t *board = json_object_get(root, "board");
    json_t *player = json_object_get(root, "currentPlayer");

    if (json_is_string(msg))
        printf("Server: %s\n", json_string_value(msg));
    
    if (json_is_string(board))
        printf("\nBoard:\n%s\n", json_string_value(board));
    
    if (json_is_string(player))
        printf("Current player: %s\n", json_string_value(player));

    json_decref(root);
}

static int callback_client(struct lws *wsi, enum lws_callback_reasons reason,
                          void *user, void *in, size_t len) {
    struct session_data *data = (struct session_data *)user;

    switch (reason) {
        case LWS_CALLBACK_CLIENT_ESTABLISHED:
            printf("Connected to server\n");
            break;

        case LWS_CALLBACK_CLIENT_RECEIVE: {
            char *payload = (char *)in;
            printf("Received: %.*s\n", (int)len, payload);
            parse_game_response(payload);
            break;
        }

        case LWS_CALLBACK_CLIENT_WRITEABLE: {
            if (move_number == 0) { // Send connection message
                char *msg = create_connection_message();
                memcpy(data->buffer + LWS_PRE, msg, strlen(msg));
                data->buffer_len = strlen(msg);
                free(msg);
                
                lws_write(wsi, data->buffer + LWS_PRE, data->buffer_len, LWS_WRITE_TEXT);
                move_number++;
            }
            break;
        }

        case LWS_CALLBACK_CLOSED:
        case LWS_CALLBACK_CLIENT_CONNECTION_ERROR:
            interrupted = 1;
            break;

        default:
            break;
    }

    return 0;
}

static struct lws_protocols protocols[] = {
    {
        "ws",
        callback_client,
        sizeof(struct session_data),
        4096,
    },
    { NULL, NULL, 0, 0 }
};

void set_nonblocking(int state) {
    struct termios ttystate;
    tcgetattr(STDIN_FILENO, &ttystate);

    if (state) {
        ttystate.c_lflag &= ~ICANON;
        ttystate.c_lflag &= ~ECHO;
        ttystate.c_cc[VMIN] = 1;
    } else {
        ttystate.c_lflag |= ICANON;
        ttystate.c_lflag |= ECHO;
    }

    tcsetattr(STDIN_FILENO, TCSANOW, &ttystate);
}

void handle_user_input(struct lws *wsi) {
    char input[256];
    if (fgets(input, sizeof(input), stdin)) {
        input[strcspn(input, "\n")] = 0; // Remove newline
        
        if (strncmp(input, "move ", 5) == 0) {
            char *notation = input + 5;
            char *color = strchr(notation, ' ');
            if (color) {
                *color = '\0';
                color++;
                char *msg = create_move_message(notation, color);
                struct session_data *data = (struct session_data *)lws_wsi_user(wsi);
                memcpy(data->buffer + LWS_PRE, msg, strlen(msg));
                data->buffer_len = strlen(msg);
                free(msg);
                lws_callback_on_writable(wsi);
            }
        }
    }
}

int main(int argc, char **argv) {
    struct lws_context_creation_info info;
    struct lws_client_connect_info connect_info;
    struct lws_context *context;
    const char *p;
    
    memset(&info, 0, sizeof info);
    info.port = CONTEXT_PORT_NO_LISTEN;
    info.protocols = protocols;
    info.gid = -1;
    info.uid = -1;

    context = lws_create_context(&info);
    if (!context) {
        fprintf(stderr, "Failed to create context\n");
        return 1;
    }

    memset(&connect_info, 0, sizeof(connect_info));
    connect_info.context = context;
    connect_info.address = SERVER_ADDRESS;
    connect_info.port = PORT;
    connect_info.path = "/";
    connect_info.host = SERVER_ADDRESS;
    connect_info.origin = SERVER_ADDRESS;
    connect_info.protocol = protocols[0].name;

    web_socket = lws_client_connect_via_info(&connect_info);
    if (!web_socket) {
        fprintf(stderr, "Connection failed\n");
        return 1;
    }

    printf("Connecting to %s:%d...\n", SERVER_ADDRESS, PORT);
    set_nonblocking(1);

    while (!interrupted) {
        lws_service(context, 50);
        handle_user_input(web_socket);
    }

    set_nonblocking(0);
    lws_context_destroy(context);
    return 0;
}
