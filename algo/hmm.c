#include <stdio.h>

int main(){
    float x = 1.0000001;
    printf("int: %0.7f\n", x);
    printf("float: %0.7f\n", x + 1.0000000);
    printf("double: %0.7f\n", x + .00000001);
}
