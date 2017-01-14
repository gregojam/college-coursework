#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(){
    FILE *file1, *file2;
    char filename[20];
    char filename2[20];
    char text1[5];
    char text2[5];

    scanf("%s %s", filename, filename2);

    file1 = fopen(filename, "r");
    printf("success1\n");
    printf("success2\n");
    file2 = fopen(filename2, "r");

    while(fscanf(file1, "%s", text1) == 1){
        fscanf(file2, "%s", text2);
        if(!strcmp(text1, text2));
        else{
            printf("false\n");
            return;
        }
    }
    fclose(file1);
    fclose(file2);

    printf("true\n");
}    
