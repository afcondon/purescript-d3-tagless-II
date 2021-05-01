# purescript-d3-tagless-II
Tagless final style interpreter / wrapper for D3 in PureScript, latest of many re-writes

![image](https://user-images.githubusercontent.com/1260895/116773970-72d05800-aa59-11eb-990e-a8a809a21d9b.png)

Proper readme to come, see readmes for my repos with previous attempts for context

Collaborators very welcome.

`>>>>>` FEEL FREE TO CONTACT ME FOR AN INTRO TO THE REPO AND IDEAS IN IT `<<<<<<`

# state of this repo

400-ish lines of interpreter code and a  minimal example showing that this interpreter is capable of implementing 
the "General Update Pattern" (ie for transitions between data sets where entering, exiting and retained data are
all separately animated), a hierarchical example (in this case a radial tree) and a force layout graph. 

The latter two examples require some kind of `httpd` in the dist directory because they're fetching JSON data to show.

The three examples have been chosen to show three very different patterns of usage at the _D3 pattern language level_ rather than for any graphical or data visualization reason.

# next steps

## Tasks and issues:
  * making the attributes work with units (ie numerical lambda but result is a string like "150px" or "0.5rem"
  * demo of force layout using output of `purs graph`
  * custom rendering for "meta tree" to better square with presentation style in PDF below
  * extend the "string instance" of the monad to generate something more like - or even, exactly like - the equivalent D3 JavaScript implementation
  * add a Sankey diagram example
  * add a drag and drop example like this stone [classic](http://bl.ocks.org/robschmuecker/7880033)
  * add a collapsable tree and / or graph example
  * demo some hover effects for graph neighborhoods
  * write a proper README!!!
  
[TaglessD3.pdf](https://github.com/afcondon/purescript-d3-tagless-II/files/6409007/TaglessD3.pdf)
