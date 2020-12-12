# CGE templates

Note that project templates itself is broken - it won't copy subdirectories so 'data' has to be copied manually ATM

1) Clone this repo to your system then start Lazarus

2) Select Tools -> Project Template Options and point the dialog that appears at this repo

3) Select File -> New Project From Template -> CGE / LCL Multi Platform Applicaton

4) Enter the 'Name for new project'

5) Give it a Directory in 'Create in directory'

6) Click OK

7) Manually copy \<repo\>\CGEApp\\data to your new directory

You can now use castle-engine compile or open \<project name\>Laz.lpi / \<project name\>App.lpi in Lazarus

