#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if(!mat) {
        return root;
    }
    if(root == NULL) {
        bst_sf *node = malloc(sizeof(bst_sf));
        if(!node) {
            return NULL;
        }
        node->mat = mat;
        node->left_child = NULL;
        node-> right_child = NULL;
        return node;
    } 

    if(mat->name < root->mat->name) {
        root->left_child = insert_bst_sf(mat, root-> left_child);
    }
    else {
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if(root == NULL) {
        return NULL;
    }
    if(name == root->mat->name) {
        return root->mat;
    }
    if(name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    }
    else {
        return find_bst_sf(name, root->right_child);
    }
    return NULL;
}

void free_bst_sf(bst_sf *root) {
    if(root == NULL) {
        return;
    }

    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);

    if(root->mat != NULL) {
        free(root->mat);
    }
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if(mat1==NULL || mat2==NULL) {
        return NULL;
    }

    if(mat1 -> num_rows != mat2 -> num_rows || mat1 -> num_cols != mat2 -> num_cols) {
        return NULL;
    }

    unsigned int r=mat1 -> num_rows;
    unsigned int c=mat1 -> num_cols;

    matrix_sf *result=malloc(sizeof(matrix_sf)+r*c*sizeof(int));
    if(result==NULL) {
        return NULL;
    }

    result -> name = '?';
    result -> num_rows = r;
    result -> num_cols = c;

    unsigned int total = r*c;
    for(unsigned int i=0; i<total; i++) {
        result -> values[i] = mat1 -> values[i] + mat2 -> values[i];
    }

    return result;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if(!mat1 || !mat2) {
        return NULL;
    }

    unsigned int r1 = mat1 -> num_rows;
    unsigned int c1 = mat1 -> num_cols;
    unsigned int r2 = mat2 -> num_rows;
    unsigned int c2 = mat2 -> num_cols;

    if(c1 != r2) {
        return NULL;
    }

    matrix_sf *result = malloc(sizeof(matrix_sf)+r1*c2*sizeof(int));
    if(!result) {
        return NULL;
    }

    result -> name = '?';
    result -> num_rows = r1;
    result -> num_cols = c2;

    for(unsigned int i=0; i<r1; i++) {
        for(unsigned int j=0; j<c2; j++) {
            int sum=0;
            for(unsigned int k=0; k<c1; k++) {
                sum += mat1 -> values[i*c1+k]*mat2 -> values[k*c2+j];
            }
            result -> values[i*c2+j] = sum;
        }
    }
   return result;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    if(!mat) {
        return NULL;
    }

    unsigned int rows = mat->num_rows;
    unsigned int cols = mat->num_cols;

    matrix_sf *result=malloc(sizeof(matrix_sf)+rows*cols*sizeof(int));
    if(!result) {
        return NULL;
    }

    result->name = '?';
    result->num_rows = cols;
    result->num_cols = rows;

    for(unsigned int i=0; i<rows; i++) {
        for(unsigned int j=0; j<cols; j++) {
            result->values[j*rows+i] = mat->values[i*cols+j];  
        }
    }
    return result;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    if(!expr) {
        return NULL;
    }

    unsigned int num_rows = 0, num_cols = 0;

    char *buffer = strdup(expr);
    if(!buffer) {
        return NULL;
    }

    char *ptr = buffer;

    while(*ptr && isspace(*ptr)) {
        ptr++;
    }
    if(!isdigit(*ptr)) {
        free(buffer);
        return NULL;
    }
    num_rows = strtoul(ptr, &ptr, 10);

    while(*ptr && isspace(*ptr)) {
        ptr++;
    }
    if(!isdigit(*ptr)) {
        free(buffer);
        return NULL;
    }
    num_cols = strtoul(ptr, &ptr, 10);

    while(*ptr && isspace(*ptr)) {
        ptr++;
    }
    if(*ptr != '[') {
        free(buffer);
        return NULL;
    }
    ptr++;

    matrix_sf *mat = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    if(!mat) {
        free(buffer);
        return NULL;
    }
    mat->name = name;
    mat->num_rows = num_rows;
    mat->num_cols = num_cols;

    unsigned int row = 0, col = 0;
    while(*ptr && row < num_rows) {
        while(*ptr && isspace(*ptr)) {
            ptr++;
        }

        if(*ptr == ';') {
            if(col != num_cols) {
                free(buffer);
                free(mat);
                return NULL;
            }
            row++;
            col = 0;
            ptr++;
            continue;
        }

        if(*ptr == ']') {
            if(row != num_rows-1 || col != num_cols) {
                free(buffer);
                free(mat);
                return NULL;
            }
            break;
        }

        char *endptr;
        int val = strtol(ptr, &endptr, 10);
        if(ptr == endptr) {
            free(buffer);
            free(mat);
            return NULL;
        }
        mat->values[row*num_cols+col] = val;
        col++;
        ptr = endptr;
    }

    free(buffer);
    return mat;
}

int precedence(char op) {
    switch(op) {
        case '\'': return 3;
        case '*': return 2;
        case '+': return 1;
        default: return 0;
    }
}

char* infix2postfix_sf(char *infix) {
    int len = strlen(infix);
    char *postfix = malloc(len*2+1);
    char stack[len];
    int top = -1;
    int k = 0;

    for(int i=0; i<len; i++) {
        char c = infix[i];

        if(isspace(c)) {
            continue;
        }

        if(isalpha(c)) {
            postfix[k++]=c;
        }
        else if(c == '(') {
            stack[++top] = c;
        }
        else if(c == ')') {
            while(top >= 0 && stack[top] != '(') {
                postfix[k++] = stack[top--];
            }
            if(top >= 0) {
                top--;
            }
        }
        else if(c == '+' || c == '*' || c == '\'') {
            while(top >= 0 && precedence(stack[top]) >= precedence(c)) {
                postfix[k++] = stack[top--];
            }
            stack[++top] = c;
        }
    }

    while(top >= 0) {
        postfix[k++] = stack[top--];
    }

    postfix[k] = '\0';
    return postfix;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    if(!expr) {
        return NULL;
    }

    char *postfix = infix2postfix_sf(expr);
    if(!postfix) {
        return NULL;
    }

    int len = strlen(postfix);
    matrix_sf **stack = malloc(len*sizeof(matrix_sf*));
    if(!stack) {
        free(postfix);
        return NULL;
    }
    
    int top = -1;
    char temp = '?';
    for(int i=0; i<len; i++) {
        char c = postfix[i];
        if(isspace(c)) {
            continue;
        }

        if(isalpha(c)) {
            matrix_sf *mat = find_bst_sf(c, root);
            if(!mat) {
                for(int j=0; j<=top; j++) {
                    free(stack[j]);
                }
                free(stack);
                free(postfix);
                return NULL;
            }
            stack[++top] = mat;
        }
        else if(c == '\'') {
            matrix_sf *m = stack[top--];
            matrix_sf *t = transpose_mat_sf(m);
            if(!t) {
                free(stack);
                free(postfix);
                return NULL;
            }
            t->name = temp++;
            stack[++top] = t;

            if(m->name >= '?') {
                free(m);
            }
        }
        else if(c == '+' || c == '*') {
            if(top < 1) {
                for(int j=0; j<=top; j++) {
                    if(stack[j]->name >= '?') {
                        free(stack[j]);
                    }
                }
                free(stack);
                free(postfix);
                return NULL;
            }

            matrix_sf *m2 = stack[top--];
            matrix_sf *m1 = stack[top--];
            matrix_sf *res;

            if(c == '+') {
                res = add_mats_sf(m1, m2);
            }
            else {
                res = mult_mats_sf(m1, m2);
            }

            res->name = temp++;
            stack[++top] = res;
            if(!isalpha(m1->name)) {
                free(m1);
            }
            if(!isalpha(m2->name)) {
                free(m2);
            }
        }
    }

    matrix_sf *result = stack[top--];
    result->name = name;

    free(stack);
    free(postfix);
    return result;
}

matrix_sf *execute_script_sf(char *filename) {
   if(!filename) {
    return NULL;
   }

   FILE *file = fopen(filename, "r");
   if(!file) {
    return NULL;
   }

   char *line = NULL;
   size_t len = 0;
   ssize_t read;

   bst_sf *root = NULL;
   matrix_sf *last_matrix = NULL;

   while((read = getline(&line, &len, file)) != -1) {
    if(read>0 && line[read-1]=='\n') {
        line[read-1] = '\0';
    }
    if(line[0] == '\0') { 
        continue;
    }

    char name = line[0];
    if(!isalpha(name) || line[1]!='=') {
        continue;
    }
    char *expr = line+2;

    while(*expr && isspace(*expr)) {
        expr++;
    }

    if(isdigit(*expr) || *expr=='[' || *expr=='-' || *expr=='+') {
        matrix_sf *mat = create_matrix_sf(name, expr);
        root = insert_bst_sf(mat, root);
        last_matrix = mat;
    }
    else {
        matrix_sf *mat = evaluate_expr_sf(name, expr, root);
        root = insert_bst_sf(mat, root);
        last_matrix = mat;
    }
   }

   free(line);
   fclose(file);
    return last_matrix;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
