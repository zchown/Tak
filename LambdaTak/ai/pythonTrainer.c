#include "pythonTrainer.h"

int connectToPython() {
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        perror("Socket creation failed");
        return -1;
    }

    struct sockaddr_in serv_addr = {
        .sin_family = AF_INET,
        .sin_port = htons(65432),
        .sin_addr.s_addr = inet_addr("127.0.0.1")
    };

    if (connect(sock, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0) {
        perror("Connection failed");
        close(sock);
        return -1;
    }

    printf("Connected to Python server\n");
    return sock;
}

void sendData(int sock, const void* data, size_t len) {
    uint32_t size = htonl((uint32_t)len);
    send(sock, &size, sizeof(size), 0);
    /* printf("Sent size indicator: %u bytes\n", (uint32_t)len); */

    size_t sent = 0;
    while (sent < len) {
        ssize_t s = send(sock, (char*)data + sent, len - sent, 0);
        if (s <= 0) {
            perror("Send failed");
            break;
        }
        sent += s;
    }
    /* printf("Sent data: %zu bytes\n", sent); */

    // Wait for acknowledgment
    waitForAck(sock);
}

void receiveData(int sock, void* buf, size_t len) {
    size_t total = 0;
    while (total < len) {
        /* printf("Waiting for %zu bytes\n", len - total); */
        ssize_t received = recv(sock, (char*)buf + total, len - total, 0);
        if (received <= 0) {
            if (received < 0) perror("Receive failed");
            /* else printf("Connection closed by peer\n"); */
            break;
        }
        total += received;
    }
    /* printf("Received %zu bytes\n", total); */
}

void sendAck(int sock) {
    char ack[] = "ACK";
    ssize_t sent = 0;
    while (sent < (strlen(ack) + 1)) {
        ssize_t s = send(sock, ack + sent, (strlen(ack) + 1) - sent, 0);
        if (s <= 0) {
            perror("Send ACK failed");
            break;
        }
        sent += s;
    }

    /* printf("Sent ACK: 'A', 'C', 'K', '\\0'\n"); */
}

int waitForAck(int sock) {
    unsigned char ack[4];
    receiveData(sock, ack, 4);

    // Debug what we actually received
    /* printf("Received ACK bytes: [%02x %02x %02x %02x]\n",  */
            /* ack[0], ack[1], ack[2], ack[3]); */

    // Check if it matches 'ACK\0'
    if (ack[0] != 'A' || ack[1] != 'C' || ack[2] != 'K' || ack[3] != '\0') {
        printf("Invalid ACK received\n");
        return 0;
    }

    /* printf("Valid ACK received\n"); */
    return 1;
}

double* pythonPredict(int sock, double* inputs, int inputSize) {
    char header[] = "predict";
    sendData(sock, header, strlen(header) + 1);
    /* printf("Sent header: %s\n", header); */

    sendData(sock, &inputSize, sizeof(int));
    /* printf("Sent input size: %d\n", inputSize); */
    sendData(sock, inputs, inputSize * sizeof(double));
    /* printf("Sent input data\n"); */

    float netbuf[OUTPUT_SIZE];
    receiveData(sock, netbuf, OUTPUT_SIZE * sizeof(float));
    /* printf("Received output data\n"); */

    double* result = malloc(OUTPUT_SIZE * sizeof(double));
    for (int i = 0; i < OUTPUT_SIZE; i++) {
        uint32_t tmp;
        memcpy(&tmp, &netbuf[i], sizeof(tmp));
        tmp = ntohl(tmp);
        float f;
        memcpy(&f, &tmp, sizeof(f));
        result[i] = (double)f;
    }
    /* printf("Converted output data to double\n"); */

    sendAck(sock);
    /* printf("Sent ACK after receiving data\n"); */
    return result;
}

void pythonTrain(int sock, double* inputs, double* outputs, int targetCount, double* targets, int dataSize) {
    char header[] = "train";
    sendData(sock, header, strlen(header) + 1);

    sendData(sock, inputs, dataSize * sizeof(double));

    sendData(sock, outputs, OUTPUT_SIZE * sizeof(double));

    sendData(sock, targets, OUTPUT_SIZE * sizeof(double));
}

void pythonTrainTD(int sock, double* inputs, double* outputs, int targetCount, double* targets, int dataSize) {
    char header[] = "trainTD";
    sendData(sock, header, strlen(header) + 1);
    
    sendData(sock, inputs, dataSize * sizeof(double));
    
    /* sendData(sock, outputs, OUTPUT_SIZE * sizeof(double)); */
    
    sendData(sock, targets, OUTPUT_SIZE * sizeof(double));
}

void closeConnection(int sock) {
    close(sock);
    printf("Connection closed\n");
}

