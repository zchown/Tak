#include "aiPlayer.h"

void sendConnectionMessage(void) {
    char msg[256];
    snprintf(msg, sizeof(msg), "{\"gameId\":\"%s\"}", GAME_ID);
    unsigned char buf[LWS_PRE + 256];
    memcpy(buf + LWS_PRE, msg, strlen(msg));
    lws_write(client_wsi, buf + LWS_PRE, strlen(msg), LWS_WRITE_TEXT);
}

const char* generateMove(const char* gameStateJson, int time) {
    printf("Generating move\n");
    json_t* board = json_object_get(json_loads(gameStateJson, 0, NULL), "board");
    if (!board) {
        fprintf(stderr, "Failed to get board\n");
        return NULL;
    }
    const char* boardStr = json_string_value(board);
    GameState* state = parseTPS(boardStr);

    if (!state) {
        fprintf(stderr, "Failed to parse TPS\n");
        return NULL;
    }
    printf("Parsed TPS\n");
    Move move = iterativeDeepeningSearch(state, time);
    char* moveStr = moveToString(&move);
    freeGameState(state);
    printf("Move: %s\n", moveStr);
    return moveStr;
}

void handleMessage(const char* msg) {
    json_error_t error;
    json_t* root = json_loads(msg, 0, &error);
    if (!root) {
        fprintf(stderr, "JSON error: %s\n", error.text);
        return;
    }

    json_t* curPlayer = json_object_get(root, "currentPlayer");
    json_t* swap = json_object_get(root, "swap");
    if (curPlayer && json_is_string(curPlayer) && swap && json_is_boolean(swap)) {
        const char* player = json_string_value(curPlayer);
        int swapFlag = json_boolean_value(swap);
        int ourTurn = 0;
        int time = 500;
        if (!swapFlag && strcmp(player, "White") == 0) {
            ourTurn = 1;
            time = 3000;
        }
        else if (swapFlag && strcmp(player, "Black") == 0) {
            ourTurn = 1;
            time = 3000;
        }
        printf("Player: %s, Our turn: %d\n", player, ourTurn);

        if (ourTurn) {
            char* move = generateMove(msg, time);
            char moveMsg[256];
            snprintf(moveMsg, sizeof(moveMsg), 
                "{\"moveGameId\":\"%s\", \"moveNotation\":\"%s\", \"moveColor\":\"%s\"}", 
                GAME_ID, move, player
            );
            free(move);
            unsigned char buf[LWS_PRE + 256];
            memcpy(buf + LWS_PRE, moveMsg, strlen(moveMsg));
            lws_write(client_wsi, buf + LWS_PRE, strlen(moveMsg), LWS_WRITE_TEXT);
        }
    }
    json_decref(root);
}

static int callbackAI(struct lws* wsi, enum lws_callback_reasons reason, void* user, void* in, size_t len) {
    switch (reason) {
        case LWS_CALLBACK_CLIENT_ESTABLISHED:
            printf("Connected to server\n");
            sendConnectionMessage();
            break;

        case LWS_CALLBACK_RECEIVE:
            printf("Received: %s\n", (char* )in);
            handleMessage((char* )in);
            break;

        case LWS_CALLBACK_CLIENT_CONNECTION_ERROR:
            fprintf(stderr, "Connection error\n");
            interrupted = 1;
            break;

        case 8: // IDK why something about the implementation of the game server
            printf("Received update \n");
            handleMessage((char* )in);
            break;
        default:
            printf("Unhandled callback reason: %d\n", reason);
            break;
    }
    return 0;
}

// Signal handler
void sigintHandler(int sig) {
    interrupted = 1;
}

int runAI() {
    struct lws_context_creation_info info;
    struct lws_client_connect_info connectInfo;
    struct lws_context* context;
    const char* address = SERVER_IP;

    memset(&info, 0, sizeof(info));
    info.port = CONTEXT_PORT_NO_LISTEN;
    info.protocols = protocols;
    info.gid = -1;
    info.uid = -1;

    signal(SIGINT, sigintHandler);

    context = lws_create_context(&info);
    if (!context) {
        fprintf(stderr, "Failed to create context\n");
        return 1;
    }

    memset(&connectInfo, 0, sizeof(connectInfo));
    connectInfo.context = context;
    connectInfo.address = address;
    connectInfo.port = PORT;
    connectInfo.host = address;
    connectInfo.origin = address;
    connectInfo.protocol = protocols[0].name;

    client_wsi = lws_client_connect_via_info(&connectInfo);
    if (!client_wsi) {
        fprintf(stderr, "Connection failed\n");
        return 1;
    }

    while (!interrupted) {
        lws_service(context, 50);
    }

    lws_context_destroy(context);
    return 0;
}
