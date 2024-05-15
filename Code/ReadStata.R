
library(rio)

convert("/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh07_all_dta/b3a_cov.dta"
        , "/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh07_all_dta/b3a_cov.csv"
        )

b3a_cov_4 = read_csv('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh07_all_dta/b3a_cov.csv')

convert("/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh07_all_dta/b3b_km.dta"
        , "/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh07_all_dta/b3b_km.csv"
)

b3a_km_4 = read_csv('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh07_all_dta/b3b_km.csv')

convert("/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh14_all_dta/b3a_cov.dta"
        , "/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh14_all_dta/b3a_cov.csv"
)

b3a_cov_5 = read_csv('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh14_all_dta/b3a_cov.csv')

convert("/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh14_all_dta/b3b_km.dta"
        , "/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh14_all_dta/b3b_km.csv"
)

b3a_km_5 = read_csv('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh14_all_dta/b3b_km.csv')
