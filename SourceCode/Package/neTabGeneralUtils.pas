//***************************************************************
// This source is written by John Kouraklis.
// © 2016, John Kouraklis
// Email : j_kour@hotmail.com
//
// The MIT License (MIT)
//
// Copyright (c) 2016 John Kouraklis
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
// KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// Unit Name: neTabGeneralUtils
//
//
//
//***************************************************************

unit neTabGeneralUtils;

interface

uses
  System.Classes, FMX.Graphics, FMX.Types, FMX.Forms;

const
  MajorVersion = '1';
  MinorVersion = '2';
  BugVersion = '0';


//***************************************************************
//
// Version History
//
//
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
//
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


procedure LoadImageFromResources(const resName: string; var image: TBitmap);

function FindParentForm(const checkComponent: TFmxObject):TForm;

implementation

uses
  System.Types;

procedure LoadImageFromResources(const resName: string; var image: TBitmap);
var
  InStream: TResourceStream;
begin
  InStream := TResourceStream.Create(HInstance, resName, RT_RCDATA);
  try
    Image.LoadFromStream(InStream);
  finally
    InStream.Free;
  end;
end;

function FindParentForm(const checkComponent: TFmxObject):TForm;
begin
  if not Assigned(checkComponent.Parent) then
    result:=checkComponent as TForm
  else
    if checkComponent.Parent Is TForm then
      result:=checkComponent.Parent as TForm
    else
      result:=FindParentForm(checkComponent.Parent.Parent);
end;


end.
