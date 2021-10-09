
# purescript-d3-tagless-II

Tagless final style interpreter / wrapper for D3 in PureScript, latest of many re-writes

Proper readme to come, see readmes for my repos with previous attempts for context

Collaborators very welcome.

`>>>>>` FEEL FREE TO CONTACT ME FOR AN INTRO TO THE REPO AND IDEAS IN IT `<<<<<<`

# to build

After cloning, you should be able to do:

* `yarn install`
* `yarn run build`
* `yarn run bundle`

and then if you serve `http` from the `dist` directory you should be able to see the demo.

# making changes to CSS

The repo uses a modified version of (several files from) the [Ocelot](https://github.com/citizennet/purescript-ocelot) ui-library from CitizenNet which have been altered to work with:

* work with TailwindCSS 2.0+
* build without webpack and other toolchain from that project

After making modifications to the CSS for this project it is necessary to `yarn run build-css` which will build and bundle a `bundle.css` file in the `dist` directory. However, it is not necessary to do this to run the demo as a working version of the css is checked into the repo.

[TaglessD3.pdf](https://github.com/afcondon/purescript-d3-tagless-II/files/6461448/TaglessD3.pdf)
