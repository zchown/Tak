#ifndef PYTHONTRAINER_H
#define PYTHONTRAINER_H

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define OUTPUT_SIZE 66

int connectToPython();
void closeConnection(int sock);

void sendData(int sock, const void* data, size_t len);
void receiveData(int sock, void* buf, size_t len);

int waitForAck(int sock);
void sendAck(int sock);

double* pythonPredict(int sock, double* inputs, int input_size);
void pythonTrain(int sock, double* inputs, double* outputs, int target_count, double* targets, int data_size);

#endif // PYTHONTRAINER_H


