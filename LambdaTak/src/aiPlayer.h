#ifndef AI_PLAYER_H
#define AI_PLAYER_H

#include <libwebsockets.h>
#include <jansson.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include "../lib/board.h"
#include "../lib/tps.h"
#include "../ai/searches.h"

#define SERVER_IP "127.0.0.1"
#define PORT 9160
#define GAME_ID "game6"

static int interrupted;
static struct lws *client_wsi;

static int callbackAI(struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len);

static struct lws_protocols protocols[] = {
    {"", callbackAI, 0, 0},
    {NULL, NULL, 0, 0}
};


void sendConnectionMessage(void);


const char* generateMove(const char* gameStateJson, int time);

void handleMessage(const char *message);

int runAI(void);

#endif // AI_PLAYER_H
