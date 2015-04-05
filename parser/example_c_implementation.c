/* 
 * File:   main.c
 * Author: hpcraink
 *
 * Created on March 24, 2015, 5:42 PM
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <err.h>
#include <errno.h>
#include <assert.h>

#define BUFFER_SIZE 65535

#define USE_FLOAT

#ifdef USE_FLOAT
#define TYPE float
#define CONVERSION "%f"
#define CONVERSION_FCT strtof
#elif USE_DOUBLE
#define TYPE double
#define CONVERSION "%lf"
#define CONVERSION_FCT strtod
#else
#error "Define either USE_FLOAT or USE_DOUBLE"
#endif

/** Read m*n-Matrix and solution vector as CSV out of file_name into A.
 * 
 * @param file_name(in)    Name of file to read Matrix from.
 * @param A(out)           Matrix A just read in (as one single allocation)
 * @param x(out)           Solution Vector x just read in (as one single allocation)
 * @param rows(out)        Number of rows of A.
 * @param columns(out)     Number of columns of A.
 * 
 * @return                 0: succeed
 *                         !=0: errno of failure
 */
int matrix_read(const char * file_name, TYPE * A[], TYPE * x[], int * rows, int * columns);

/** Gaussian Elimination of Matrix A
 * 
 * @param A(inout)     Input of Matrix A
 * @param n            Number of rows and columns of A.
 * 
 * @return             0: Elimination succeed   
 *                     otherwise: errno of failure
 */
int matrix_gaussian_elimination(TYPE * A[], TYPE * x[], int rows, int columns);

/** Helper function for white-space removal
 * 
 * Ignore white space (space / tabs) starting at begin until end
 * @param begin(inout) Beginning of string
 * @param end(in)      Ending address of string
 */
void ignore_whitespace(char ** begin, const char * end) {
    char * p = *begin;
    while ((*p == ' ' || *p == '\t') && p < end)
        p++;
    *begin = p;
}

int matrix_read(const char * file_name, TYPE * A[], TYPE * x[], int * rows, int * columns) {
    char ERROR_STRING[256] = "";
    struct stat fs;
    int line = 0;
    int matrix_row = 0;
    int matrix_done = 0;
    int vector_row = 0;
    int vector_done = 0;
    int r = 0;
    int c = 0;

    assert(NULL != file_name);
    assert(NULL != A);
    assert(NULL != x);
    assert(NULL != rows);
    assert(NULL != columns);

    /* Initialize to something sane */
    *rows = 0;
    *columns = 0;

    /* Open the CSV file for reading */
    int fd = open(file_name, O_RDONLY);
    if (-1 == fd)
        goto error_file;

    if (-1 == fstat(fd, &fs))
        goto error_file;

    char * buf = mmap(0, fs.st_size, PROT_READ, MAP_SHARED, fd, 0);
    if ((void*) - 1 == buf) {
        goto error_close;
    }

    const char * buf_end = buf + fs.st_size;
    char * begin;
    char * end;

    begin = end = buf;

    while (end <= buf_end) {
        // As long as we have not found a EOL, count
        while (!('\r' == *end || '\n' == *end)) {
            if (++end >= buf_end)
                break;
        }
        if ((1 + end) < buf_end) {
            char ch = *(1 + end);
            if (('\r' == ch || '\n' == ch) && ch != *end)
                ++end;
        }

        // Ignore White-Space :
        ignore_whitespace(&begin, end);

        // If we parse a comment sign, goto next line, also for empty lines
        if ('#' == *begin || '\n' == *begin) {
            begin = ++end;
            line++;
            continue;
        }
        // First, we need to parse for rows, then for Columns, then the matrix.
        if (0 == r) {
            r = strtol(begin, (char **) NULL, 10);
            if (r <= 0) {
                snprintf(ERROR_STRING, sizeof (ERROR_STRING),
                        "Expected positive Integer value for value ROW at line:%d",
                        line + 1);
                errno = EINVAL;
                goto error_parse;
            }
        } else if (0 == c) {
            c = strtol(begin, (char **) NULL, 10);
            if (c <= 0) {
                snprintf(ERROR_STRING, sizeof (ERROR_STRING),
                        "Expected positive Integer value for value COLUMN at line:%d",
                        line + 1);
                errno = EINVAL;
                goto error_parse;
            }

            // Allocate the matrix and solution vector;
            *A = (TYPE *) malloc(r * c * sizeof (TYPE));
            if (NULL == *A)
                goto error_close;

            *x = (TYPE *) malloc(r * sizeof (TYPE));
            if (NULL == *x)
                goto error_close;
        } else if (!matrix_done) {
            TYPE val;
            char * val_begin = begin;
            char * val_end = val_begin;
            for (int cols = 0; cols < c; cols++) {
                ignore_whitespace(&val_begin, end);
                // The conversion function (strtod / strtof) would swallow
                // erroneous new lines... So abort for them!
                if ('\n' == *val_begin) {
                    snprintf(ERROR_STRING, sizeof (ERROR_STRING),
                            "Expected Floating Point value at line:%d column:%d",
                            line + 1, (int) (val_begin - begin));
                    errno = EINVAL;
                    goto error_parse;
                }
                val = CONVERSION_FCT(val_begin, &val_end);
                // Check for error:
                if (0.0 == val && val_end == val_begin) {
                    snprintf(ERROR_STRING, sizeof (ERROR_STRING),
                            "Expected Floating Point value at line:%d column:%d",
                            line + 1, (int) (val_begin - begin));
                    errno = EINVAL;
                    goto error_parse;
                }

                (*A)[matrix_row * c + cols] = val;

                val_begin = val_end;
                ignore_whitespace(&val_begin, end);
                // Except for the last column, we expect a comma.
                // For the last column, we do expect a newline...
                if (cols != (c - 1) && ',' != *val_begin) {
                    snprintf(ERROR_STRING, sizeof (ERROR_STRING),
                            "Expected comma at line:%d column:%d (expected %d columns, but read %d)",
                            line + 1, (int) (val_begin - begin), c, cols + 1);
                    errno = EINVAL;
                    goto error_parse;
                } else if (cols == (c - 1) && '\n' != *val_begin) {
                    snprintf(ERROR_STRING, sizeof (ERROR_STRING),
                            "Expected newline at line:%d column:%d (expected %d columns, but read %d)",
                            line + 1, (int) (val_begin - begin), c, cols + 1);
                    errno = EINVAL;
                    goto error_parse;
                }
                val_end = ++val_begin;
            }
            // If we have read all rows, we may exit
            if (++matrix_row == r)
                matrix_done = 1;

        } else if (!vector_done) {
            TYPE val;
            char * val_end;
            val = CONVERSION_FCT(begin, &val_end);
            // Check for error:
            if (0.0 == val && val_end == begin) {
                snprintf(ERROR_STRING, sizeof (ERROR_STRING),
                        "Expected Floating Point value for vector at line:%d",
                        line + 1);
                errno = EINVAL;
                goto error_parse;
            }

            (*x)[vector_row] = val;
            if (++vector_row == r)
                vector_done = 1;
        }

        // Start with next line -- unless we're past last line.
        begin = ++end;
        line++;
    }

    *rows = r;
    *columns = c;
    return 0;

error_parse:
    fprintf(stderr, "%s\n", ERROR_STRING);
error_close:
    close(fd);
error_file:
    return errno;
}

int main(int argc, char * argv[]) {
    int ret;
    int rows, columns;
    TYPE * A, c, * x;

    ret = matrix_read("matrix_3x3.csv", &A, &x, &rows, &columns);
    if (0 != ret)
        return -1;

    for (int i = 0; i < rows; i++) {
        printf("A[%d]", i);
        for (int j = 0; j < columns; j++) {
            printf(CONVERSION " ", A[i * columns + j]);
        }
        printf(" x[%d]: " CONVERSION "\n", i, x[i]);
    }

    // matrix_gaussian_elimination(A, x, rows, columns);

    /*
    printf("\nThe solution is: \n");
    for (i = 1; i <= n; i++) {
        printf("\nx%d=%f\t", i, x[i]);
    }
     */
    return 0;
}