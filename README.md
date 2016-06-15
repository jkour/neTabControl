# neTabControl
neTabControl is a FireMonkey control for Delphi. It builds on the native TabControl and adds a number of features.

> **Important note:** Starting from v.1.0.0, neTabControl is not a simple subclass of TTabControl as in previous versions. If you use previous versions you need to update the control in the forms you're using it

#Design
The control uses three elements:

1. A Tag
2. A TneTabItem: An ancestor of TTabItem
3. A Frame

The neTabControl holds several TabItems, which all inherit from TneTabItem and shows a TFrame for each TneTabItem.

For more details about the Architecture of neTabControl, check the wiki.

#How To Use

Build and install the package: Open the Package file and install the component. Then you can drag and drop it in any forms

**Note:** The actual components of each tab need to be embedded in a Frame and then attached to the tabControl

#FMX Style
Starting from version 1.0.0 neTabControl doesn't use styles to embed the close image.

#Highlights

* Tab items are linked to tags instead of indices. 
For example you can write: SetActiveTab("Welcome Screen") or DeleteTab('Report Summary')
* TabItems have close icon
* TabItems can embed any control that descends from TControl (eg, image, progress bar, etc.) [v.1.0.0]
* Ability to add buttons before or after the tab items (eg. Add button, etc.) [v.1.0.0]
* Style-indenendent tab item control. You can use any pre-existing style file without any customisation. neTabControl embeds the close image and the other control automatically
* Customisable Pop-up menu
* Procedure to export the list of open tabs in order to implement load/save workspace feature

#Screenshots

Customisable Tab Bar

[![](https://github.com/jkour/neTabControl/blob/master/Screenshots/TabBar.png)](https://github.com/jkour/neTabControl/blob/master/Screenshots/TabBar.png)

Close Image On Mouse Hover


Customisable Right-click menu


#Documentation
There is documentation describing the methods and properties. Please check the "Documentation" folder.

#Bugs, Suggestions, Comments and General Contact
I hope you find the component useful. If you have any comments, spotted bug or changes to the code use the repository or drop me an email at j_kour@hotmail.com

Thanks!
 

