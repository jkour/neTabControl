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
Note: The actual components of each tab need to be embedded in a Frame and then attached to the tabControl

