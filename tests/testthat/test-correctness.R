
testthat::context(desc = "correctness test suite");

###################################################
test.correctness <- function() {
    my.seed      <- 1111111; # 7654321;
    my.tolerance <- ifelse("windows" == base::.Platform[["OS.type"]],1e-3,1e-6);
    test.correctness_json();
    }

###################################################
test.correctness_json <- function() {

    cat("\ntest.correctness_json(): normalizePath(getwd())\n")
    print( normalizePath(getwd()) );

    file.stems <- base::c(
        "Industry-Classification-Survey",
        "CSBC4-2021",
        "flow-extraction",
        "OID-exit"
        );

    for ( file.stem in file.stems ) {

        expected.txt <- base::paste0('expected-',file.stem,'.txt');
        expected.txt <- base::system.file("extdata", expected.txt, package = "stcEQDT");

        computed.txt <- base::paste0('computed-',file.stem,'.txt');
        json2tree(
            eqdt.json        = base::system.file("extdata", base::paste0(file.stem,".json"), package = "stcEQDT"),
            file.output      = computed.txt,
            output.directory = NULL
            );

        testthat::test_that(
            desc = base::paste0("json2tree correctness test: ",file.stem),
            code = {
                testthat::expect_identical(
                    object   = base::as.character(tools::md5sum(computed.txt)),
                    expected = base::as.character(tools::md5sum(expected.txt))
                    );
                }
            );

        }

    }

###################################################
test.correctness();
