# research-blog-posts
This project includes the code that Euclidean used to generate the
charts in our data (blog) posts at http://www.euclidean.com/data-posts/.
Except for the very first blog post, all of the R scripts that generate
the charts are fully self contained. Meaning that you don't need to
prepare a data source for the scripts.
Even though the scripts depend on data, the data
is either downloaded by the script from a website or the module comes with the
required data file. The only dependancies that you must take care of is
installing any of the R libraries that the script requires. 
Each sub directory of this project corresponds to
a single blog post. Most scripts have the name chart1.R, chart2.R,
etc. -- corresponding to the order in the blog post that they appear.
