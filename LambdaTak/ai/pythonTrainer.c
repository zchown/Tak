#include "pythonTrainer.h"

int connect_to_python() {
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
        /* printf("Invalid ACK received\n"); */
        return 0;
    }

    /* printf("Valid ACK received\n"); */
    return 1;
}

double* pythonPredict(int sock, double* inputs, int input_size) {
    /* printf("\n--- STARTING PREDICTION REQUEST ---\n"); */
    /* printf("Sending prediction request with input size %d\n", input_size); */

    // Send prediction request header
    char header[] = "predict";
    sendData(sock, header, strlen(header) + 1);

    // Send input size
    sendData(sock, &input_size, sizeof(int));

    // Send input data
    sendData(sock, inputs, input_size * sizeof(double));

    // Receive prediction response (4-byte float in network byte order)
    float prediction_value_network;
    receiveData(sock, &prediction_value_network, sizeof(float));

    // Convert network byte order to host byte order if needed
    // (handling the float representation)
    uint32_t* as_int = (uint32_t*)&prediction_value_network;
    *as_int = ntohl(*as_int);
    float prediction_value = *(float*)as_int;

    /* printf("Received prediction value: %f\n", prediction_value); */

    // Send acknowledgment
    sendAck(sock);

    // Allocate memory for the result and return it
    double* result = (double*)malloc(sizeof(double));
    if (result == NULL) {
        perror("Memory allocation failed");
        return NULL;
    }
    *result = (double)prediction_value;

    /* printf("--- PREDICTION REQUEST COMPLETED ---\n\n"); */
    return result;
}

void pythonTrain(int sock, double* inputs, double* outputs, int target_count, double* targets, int data_size) {
    /* printf("\n--- STARTING TRAINING REQUEST ---\n"); */
    /* printf("Sending training request with data size %d\n", data_size); */

    // Send request type
    char header[] = "train";
    sendData(sock, header, strlen(header) + 1);

    // Send input data
    sendData(sock, inputs, data_size * sizeof(double));

    // Send current outputs
    sendData(sock, outputs, target_count * sizeof(double));

    // Send target values
    sendData(sock, targets, target_count * sizeof(double));

    /* printf("--- TRAINING REQUEST COMPLETED ---\n\n"); */
}

void closeConnection(int sock) {
    close(sock);
    printf("Connection closed\n");
}
