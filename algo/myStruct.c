#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int *nums, *tmp, min, max;

void trim(char *str){
    char *end;

    // trim leading whitespace
    while(isspace((unsigned char) *str)) str++;

    if(*str == 0)  // all whitespace string
        return;

    // trim trailing whitespace
    end = str + strlen(str) - 1;
    while(end > str && isspace((unsigned char) *end)) end--;

    // new null
    *(end+1) = 0;
}

int insert(int num){
    int i;
    if(num < 0 || num > 1000000)
        printf("Invalid number\n");
    else{
        if(max < 0){ // empty array
            min = num;
            max = num;
            nums = malloc(sizeof(int));
            nums[0] = 1;
        }
        else if(num > max){ // grow after
            tmp = malloc((num-min) * sizeof(int));
            memset(tmp, 0, (num-min) * sizeof(int));
            for(i = 0; i <= max-min; i++){
                tmp[i] = nums[i];
            }
            max = num;
            tmp[max-min] = 1;
            nums = tmp;
        }
        else if(num < min){ // grow before
            tmp = malloc((max-num) * sizeof(int));
            memset(tmp, 0, (max-num) * sizeof(int));
            for(i = 0; i <= max-min; i++){
                tmp[i+min-num] = nums[i];
            }
            min = num;
            tmp[0] = 1;
            nums = tmp;
        }
        else // increment count
            nums[num-min]++;
    }
}


int delete(int num){
    int i;
    if(num < min || num > max); // Do nothing
    else{
        if(nums[num-min]) // if there, decrement counter
            nums[num-min]--;
        if(num == min){ // need to shrink before
            while(nums[min-min] == 0 && min < max){
                min++;
            }
            tmp = malloc((max-min) * sizeof(int));
            for(i = 0; i <= max-min; i++){
                tmp[i] = nums[i+min-num];
            }
            nums = tmp;
        }
        if(num == max){ // need to shrink after
            while(nums[max-min] == 0 && min < max){
                max--;
            }
            tmp = malloc((max-min) * sizeof(int));
            for(i = 0; i <= max-min; i++){
                tmp[i] = nums[i];
            }
            nums = tmp;
        }
        if(min == max && nums[0] == 0){
            min = -1;
            max = -1;
            free(nums);
        }
    }
}

int member(int num){
    if(num < min || num > max)
        return 0;
    else if(nums[num-min])
        return 1;
    else
        return 0;
}

int count(int num){
    if(num < min || num > max)
        return 0;
    else
        return nums[num-min];
}

int sum(int num){
    int i;
    if(num < min || num > 2 * max)
        return 0;
    else{
        for(i = 0; i <= max-min; i++){
            if(member(i+min) && member(num-i-min)){
                if(i+min == num-i-min){
                    if(count(i+min) > 1)
                        return 1;
                }
                else
                    return 1;
            }
            if(i == max-min)
                return 0;
        }
    }
}

void print(){
    int i;
    printf("[");
    if(max < 0)
        printf("NIL");
    else{
        for(i = 0; i <= max-min; i++){
            printf("%d", nums[i]);
            if(i < max-min)
                printf(", ");
        }
    }
    printf("]\n");
}

void minmax(){
    printf("min: %d, max: %d\n", min, max);
}

void readfile(char *filename){
    FILE *ifile, *ofile;
    char func[10], *token;
    int num;

    ifile = fopen(filename, "r");

    strtok(filename, ".");
    strcat(filename, ".res");
    if(ifile){
        ofile = fopen(filename, "w+");
        while(fscanf(ifile, "%s", func) == 1){
            token = strtok(func, "(");
            token = strtok(NULL, ")");
            num = atoi(token);
            if(!strcmp(func, "I")){
                insert(num);
                fprintf(ofile, "i ");
            }

            else if(!strcmp(func, "D")){
                delete(num);
                fprintf(ofile, "d ");
            }

            else if(!strcmp(func, "M")){
                if(member(num))
                    fprintf(ofile, "yes ");
                else
                    fprintf(ofile, "no ");
            }

            else if(!strcmp(func, "C"))
                fprintf(ofile, "%d ", count(num));

            else if(!strcmp(func, "S")){
                if(sum(num))
                    fprintf(ofile, "yes ");
                else
                    fprintf(ofile, "no ");
            } 
        }
    fclose(ofile);
    fclose(ifile);
    }
    else
        printf("Could not find file\n");

}


void help(){
    printf( "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
            "insert -> insert number into array\n"
            "delete -> delete number from array\n"
            "member? -> is number in array?\n"
            "count -> how many of a number in array?\n"
            "sum? -> do two numbers in array sum to number?\n"
            "print -> print number of each element in array\n"
            "minmax -> print minimum and maximum value in array\n"
            "file -> read input file; write results in .res file\n"
            "help -> bring up this menu here\n"
            "quit -> exit program\n"
            "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n");
}

int manual(){
    char input[15];
    int num, i;
    char file[50];

    while(1){
        printf("$>> ");
        fgets(input, 15, stdin);
        trim(input);

        if(!strcmp(input, "insert")){ // insert
            printf("Number to insert: ");
            scanf("%d", &num);
            getchar();
            insert(num);
        }

        else if(!strcmp(input, "delete")){ // delete
            printf("Number to delete: ");
            scanf("%d", &num);
            getchar();
            delete(num);
        }

        else if(!strcmp(input, "member?")){ // check for membership
            printf("Number to check: ");
            scanf("%d", &num);
            getchar();
            if(member(num))
                printf("yes\n");
            else
                printf("no\n");
        }

        else if(!strcmp(input, "count")){ // count number
            printf("Number to count: ");
            scanf("%d", &num);
            getchar();
            printf("%d\n", count(num));
        }

        else if(!strcmp(input, "sum?")){ // i + j = num?
            printf("Number to sum: ");
            scanf("%d", &num);
            getchar();
            if(sum(num))
                printf("yes\n");
            else
                printf("no\n"); 
        }

        else if(!strcmp(input, "print")){ // print count of each present
            print(); 
        }

        else if(!strcmp(input, "minmax")) // print min and max
            minmax();

        else if(!strcmp(input, "file")){ // run file
            printf("Name of file: ");
            scanf("%s", &file);
            getchar();
            readfile(file);
        }

        else if(!strcmp(input, "help")) // print help menu
            help();

        else if(!strcmp(input, "quit")) // exit program
            return;

    }
}

int main(){
    min = -1;
    max = -1;

    help();
    manual();
}
