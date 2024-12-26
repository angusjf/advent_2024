#import <stdio.h>

// desired = [2, 4, 1, 1,7,5,1,5,0,3,4,3,5,5,3,0]
                      // 7,5,1,5,0,3,4,3,5,5,3,0,

 long output(long a) {
    return a % 8 ^ 1 ^ 5 ^ (a / (1 << (a % 8 ^ 1))) % 8;
}

int main() {
    // long too_low = 140737488355328; // too low
    // long too_high = 178388203060038; // too high
    long too_lo = 164542125272764;
    long too_hi = 164542125272766;

    for (long cand = too_lo; cand < too_hi; cand += 1) {
        long a = cand;
        printf("\n%ld: ", a);
        while (1) {
            printf("%ld,", output(a));

            a /= 8;

            if (a == 0) break;
        }
    }
}
