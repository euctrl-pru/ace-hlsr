
# hlsr_ace

This repo contains the material to generate a bookdown version of the ACE High-level summary report.

## Rendering

To compile the whole doc in its HTML output, you can execute in the terminal:

```
quarto render --to html

```

For PDF output, execute in the terminal:

```
quarto render --to pdf

```

Both commands above will render the output in the '_book' folder.

Note that when rendering in one format, it will erase the files from the other format, so if you want to keep a copy of your .pdf, save a copy somewhere outside the '_book' before rendering the html.

## Yearly update

1. For the yearly update, create a branch 'hlsr-202x' where 202x is the year the hlsr reports on.
2. Update the variables.yml file. Typically add 1 year to all variables.
3. Make the updates as requested by the ECO team.
4. Set up netlify to get a url for the preview

  4.1. Open your ace-hlsr project in Netlify.

  4.2. Go to Project configuration → Developer settings → Continuous deployment → Branches and deploy contexts → Configure.

  4.3. Under Branch deploys, select Let me add individual branches and add hlsr-202x.

  4.4. Save, keeping your existing production branch unchanged. 

  5. Then, on your local hlsr-202x branch:

5.1. Render the book as usual.

5.2. Commit the rendered HTML and its supporting files—images, CSS, JavaScript, and Plotly dependencies.

5.3. Push the branch: `git push -u origin hlsr-202x`


Once the branch deployment succeeds, its expected URL is:

https://hlsr-202x--ace-hlsr.netlify.app/
