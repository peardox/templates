The following has been tested on Window 10, Linux (Ubuntu 20.04 LTS)
and Mac OSX 10.15.5 - all function identically.

This is a CGE 3D Hello World template. The solution presented uses the
dreadful pre-installed project templates as a quick proof of concept.
The aim is to have one easily maintainable template repository that is
both Castle-Editor and Lazarus friendly.

The project template system is buggy so it is currently required that 
you manually copy the templates/Trivial/data directory to the newly
created project's location as it doesn't correctly recurse the template.

To install the test template start Lazarus select 
Tools -> Project templates options...

Use the file Browser dialog to select the templates directory (the one
with this file in it) and click OK.

Now select Project -> New Project...

At the bottom of the list you'll now see CGE LCL Application so select 
that option and click OK

Fill in the form - no spaces (buggy) additionally selecting a directory
for the new project (remember this path - you're about to need it).

Click OK and your project will be created (it won't work, but at least
most of the code is present)

Before attempting to run the test app manually copy the data directory from
the templates directory (as noted at the start) into your new project
directory.

Another bug can be seen if you select Project -> Project inspector...
In this case the dependancies are wrong - note there's no CGE and there's
an FCL entry (which isn't used).

You can now run the test app. A dialog box will pop up informinf you that
the lpr file has changed so click the pre-selected 'Reload...' button and
everything will resolve itself.

The UI is horrible, the recursion doesn't and the end result is initially 
loaded poorly but with some fiddling around at least it works and can,
of course, be vastly improved.