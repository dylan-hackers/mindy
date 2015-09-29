SET(dylan-test_SOURCES
    dylan-test-exports.dylan
    dylan-test.dylan
)
ADD_DYLAN_TEST(dylan-test)

SET(regexp-test_SOURCES
    regexp-test-exports.dylan
    regexp-test.dylan
)
ADD_DYLAN_TEST(regexp-test)

SET(stream-test_SOURCES
    stream-test-exports.dylan
    stream-test.dylan
)
ADD_DYLAN_TEST(stream-test)

SET(table-ext-test_SOURCES
    table-ext-test-exports.dylan
    table-ext-test.dylan
)
ADD_DYLAN_TEST(table-ext-test)

SET(random-test_SOURCES
    random-test-exports.dylan
    random-test.dylan
)
ADD_DYLAN_TEST(random-test)

# SET(matrix-test_SOURCES
#     matrix-test-exports.dylan
#     matrix-test.dylan
# )
# ADD_DYLAN_TEST(matrix-test)

SET(coll-ext-test_SOURCES
    coll-ext-test-exports.dylan
    coll-ext-test.dylan
)
ADD_DYLAN_TEST(coll-ext-test)

SET(print-test_SOURCES
    print-test-exports.dylan
    print-test.dylan
)
ADD_DYLAN_TEST(print-test)

SET(format-test_SOURCES
    format-test-exports.dylan
    format-test.dylan
)
ADD_DYLAN_TEST(format-test)

# SET(format-out-test_SOURCES
#     format-out-test-exports.dylan
#     format-out-test.dylan
# )
# ADD_DYLAN_TEST(format-out-test)

# SET(getopttest_SOURCES
#     getopt-test-exports.dylan
#     getopt-test.dylan
# )
# ADD_DYLAN_TEST(getopttest)
