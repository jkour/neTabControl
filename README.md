# neTabControl
neTabControl is a FireMonkey control for Delphi. It extends the native TabControl and adds a number of features.

#Design
The control is built on three elements:

1. A Tag
2. A TneTabItem: An ancestor of TTabItem
3. A Frame

The neTabControl holds several TabItems, which all inherit from TneTabItem and shows a TFrame for each TneTabItem.

#How To Use

There are two ways to use the component:

1. Build and install the package: Open the Package file and install the component. Then you can drag and drop it in any forms

2. Open the Project Group and run the Demo to see how to use the component in run-time mode
<b>Note:</b> The actual components of each tab need to be embedded in a Frame and then attached to the tabControl

#FMX Style
There is a style file (neTabControlStyle) which is used in demo to style the tab item and the hint layer.


#Features

* Tab items are linked to tags instead of indices. For example you can write: SetActiveTab("Welcome Screen") or DeleteTab('Report Summary')
* TabItems have close icon
* TabItems have additional icon
* Customisable Pop-up menu
* Hint can show the title of the tab item
* Hint can show a preview of the whole tab item
* Procedure to export the list of open tabs in order to implement load/save workspace feature

#Screenshots

Customisable Tab Bar
![alt text](https://github.com/jkour/neTabControl/blob/master/Screenshots/TabBar.png "Tab Bar")

Close Image On Mouse Hover
![alt text](https://github.com/jkour/neTabControl/blob/master/Screenshots/CloseImageOnHover.png "Close Image On Hover")

Title as Hint
![alt text](https://github.com/jkour/neTabControl/blob/master/Screenshots/TitleAsHint.png "Title As Hint")

Preview as Hint
![alt text](https://github.com/jkour/neTabControl/blob/master/Screenshots/PreviewAsHint.png "Preview As Hint")

Customisable Right-click menu
![alt text](https://github.com/jkour/neTabControl/blob/master/Screenshots/Right-Click.png "Customisable Right-click menu")


#Documentation
There is documentation describing the methods and properties. Please check the "Documentations" folder.

#Bugs, Suggestions, Comments and General Contact
I hope you find the component useful. If you have any comments, spotted bug or changes to the code use the repository or drop me an email at j_kour@hotmail.com

Thanks!
 

