
# hlsr_ace

<!-- badges: start -->
<!-- badges: end -->

This repo contains the material to generate a bookdown version of the ACE High-level summary report.

To compile the whole doc in its HTML output, you can execute

Test change for freeze issue

```
bookdown::render_book(input = 'index.Rmd', output_format = 'bookdown::gitbook')

# for Bootstrap4 version
bookdown::render_book(input = 'index.Rmd', output_format = 'bookdown::bs4_book')

```

