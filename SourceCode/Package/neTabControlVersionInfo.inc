//***************************************************************
//
// Version History
//
//
// 1.4.1 - 21/11/2017
//
//** Improvement
//
//    * Fix an access violation with one tab in the control
//    * Fix memory leaks
//
// 1.4.0 - 12/10/2016
//
//** New Features
//
//    * Add Animation to tabs
//    * Change the location of version info
//
// 1.3.1 - 02/10/2016
//
//** Improvement
//
//    * Wrong arguments to AddTab raise exceptions with
//      information message
//
// 1.3.0 - 24/09/2016
//
//** New Features
//
//    * ControlAlignment, CaptionAlignment, CloseAlignment properties added
//    * Access to the text element of a TabItem
//    * TabPosition property added (see below for the renaming of the old
//      TabPosition property)
//
//** Improvement
//
//    * The TabPosition property in previous releases is renamed to
//      TabOrientation
//
// 1.2.0 - 21/08/2016
//
//** Bug
//    * Access Violation when the mouse hovered over close image on MacOS
//      and Win64 (This fixed a known issue in 1.1.1)
//    * fActiveTab was never set
//
//** New Features
//
//    * GetFrame method added
//    * OnBeforeCloseAllItems, OnAfterCloseAllItems, OnBeforeCloseOtherItems,
//      OnAfterCloseOtherItems events added
//    * DeleteAllTabs procedure added
//    * Tabs[index] property added

// 1.1.1 - 09/07/2016
//
//** Improvement
//
//    * Observer framework renamed
//
// 1.1.0 - 09/07/2016
//
//** Improvement
//
//    * Installer recognises Delphi installation and targets
//
// 1.0.0 - 15/06/2016
//
// Note: This is a code-break update. The neTabControl is not based solely
// on the TabControl as in the previous versions
// The component is rewritten from scratch to provide new features
//
//** New Features
//    * Option to place the tab items at the top and the left side of the control
//    * Option to add buttons before or after the tab items
//    * Style-independent tab items: The tab items don't need a modified style
//      to hold the close image and the other controls
//    * You can embed any type of control (TControl descendent) in a tabitem
//      and not only an image as in previous versions
//
// 0.3.0 - 28/03/2016
//
//** New Features
//    * DeleteAllTabs procedure added.
//    * Method to find the parent neTabItem of a frame
//    * TagValue property added to neTabItem
//    * ActiveTag property Added
//    * neTabItem has an Identifier property
//    * Add ability to save tabs
//
//** Improvement
//    * Code clean up
//    * Title of Tab and Preview appear as Hint
//    * Spelling mistake: MixTabWidth instead of MinTabWidth
//
//**Bug
//    * Before/After popup menus didn't trigger OnClick events
//
//
// 0.2.0 - 05/03/2016
//
//** Changes
//    * Change license to MIT
//    * ShowHint disabled
//    * Check for duplicate Tag name added
//    * Adjust Width improvement
//
//** Bug
//    * Access Violation on MouseOverTabItem when the control is
//      host in other control (eg. a Frame)
//    * AdjustWidth returns 0 when the control is within two
//      frames
//
//** New Feature
//    * Option to disable right-click menu added
//    * MinWidth property added
//    * Normal TTabItem can be added
//    * Add "Close all the rest" option in popup menu
//
//
// 0.1.0 - Initial Version (20/02/2016)


{$HINTS OFF}
const
  MajorVersion = '1';
  MinorVersion = '4';
  BugVersion = '1';
{$HINTS ON}
