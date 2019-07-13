#ifndef CCCC_ASSERT_H
#define CCCC_ASSERT_H

#include <stdlib.h>

#define assert(cond) \
    if (cond) {      \
    } else           \
        abort();

#endif  // CCCC_ASSERT_H

