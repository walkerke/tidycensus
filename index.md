# tidycensus <img src=logo.png width = "250px" align = "right">

[![R build status](https://github.com/walkerke/tidycensus/workflows/R-CMD-check/badge.svg)](https://github.com/walkerke/tidycensus/actions) ![CRAN Badge](http://www.r-pkg.org/badges/version/tidycensus)  ![CRAN Downloads](http://cranlogs.r-pkg.org/badges/tidycensus)

__tidycensus__ is an R package that allows users to interface with the US Census Bureau's decennial Census and five-year American Community APIs and return tidyverse-ready data frames, optionally with simple feature geometry included.  Install from CRAN with the following command: 

```r
install.packages("tidycensus")
```

__tidycensus__ is designed to help R users get Census data that is pre-prepared for exploration within the __tidyverse__, and optionally spatially with __sf__.  To learn more about how the package works, plase read through the following articles: 

* [Basic usage of __tidycensus__](articles/basic-usage.html)
* [Spatial data in __tidycensus__](articles/spatial-data.html)
* [Margins of error in the ACS](articles/margins-of-error.html)
* [Other Census Bureau datasets](articles/other-datasets.html)
* [Working with Census microdata](articles/pums-data.html)

## Future development

To keep up with on-going development of __tidycensus__ and get even more examples of how to use the package, subscribe to the Walker Data email list below.  You'll also get updates on the forthcoming CRC Press book _Analyzing the US Census with R_, which will cover a wide range of Census data analysis applications.   

<!-- Begin MailChimp Signup Form -->
<link href="//cdn-images.mailchimp.com/embedcode/slim-10_7.css" rel="stylesheet" type="text/css">
<style type="text/css">
#mc_embed_signup{background:#fff; clear:left; font:14px Helvetica,Arial,sans-serif; }
/* Add your own MailChimp form style overrides in your site stylesheet or in this style block.
We recommend moving this block and the preceding CSS link to the HEAD of your HTML file. */
</style>
<div id="mc_embed_signup">
<form action="//github.us15.list-manage.com/subscribe/post?u=1829a68a5eda3d301119fdcd6&amp;id=c4a53d2961" method="post" id="mc-embedded-subscribe-form" name="mc-embedded-subscribe-form" class="validate" target="_blank" novalidate>
<div id="mc_embed_signup_scroll">

<input type="email" value="" name="EMAIL" class="email" id="mce-EMAIL" placeholder="email address" required>
<!-- real people should not fill this in and expect good things - do not remove this or risk form bot signups-->
<div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_1829a68a5eda3d301119fdcd6_c4a53d2961" tabindex="-1" value=""></div>
<div class="clear"><input type="submit" value="Subscribe" name="subscribe" id="mc-embedded-subscribe" class="button"></div>
</div>
</form>
</div>

<!--End mc_embed_signup-->

While tidycensus focuses on a select number of US Census Bureau datasets, there are many others available via the Census Bureau API.  For access to all of these APIs, please check out Hannah Recht's excellent [censusapi package](https://github.com/hrecht/censusapi). 

If you find tidycensus useful in your work and would like to ensure continued development of the package, you can provide support in the following ways: 

* [Chip in some funds to support package development via PayPal](https://paypal.me/walkerdata);
* Set up a consulting engagement or workshop though Walker Data to help you implement tidycensus in your project.  Send a note to <kyle@walker-data.com> if you are interested; 
* File an issue - or even better, a pull request - at https://github.com/walkerke/tidycensus/issues.  

Note: This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.


