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
// Unit Name: neTabTypes
//
//
//
//***************************************************************

unit neTabTypes;

interface

uses
  FMX.Types, Model.IntActions, Model.Interf,
  System.Types, FMX.Menus, System.Classes, System.Generics.Collections,
  FMX.TabControl;

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



type

  {$REGION 'Defines the elements of a tab item to be exported with the SaveTabs procedure'}
  /// <summary>
  ///   Defines the elements of a tab item to be exported with the SaveTabs
  ///   procedure
  /// </summary>
  /// <seealso cref="TExportElements">
  ///   <see cref="neTabTypes|TExportElements" />
  /// </seealso>
  {$ENDREGION}
  TExportElement = (
    {$REGION 'Export the Title of the tab item'}
    /// <summary>
    ///   Export the Title of the tab item
    /// </summary>
    {$ENDREGION}
    expTitle,
    {$REGION 'Exports the TagString property'}
    /// <summary>
    ///   Exports the TagString property
    /// </summary>
    {$ENDREGION}
    expTagString,
    {$REGION 'Exports the TagFloat property'}
    /// <summary>
    ///   Exports the TagFloat property
    /// </summary>
    {$ENDREGION}
    expTagFloat);
  {$REGION 'Defines the set of elements to be exported when SaveTabs is called'}
  /// <summary>
  ///   Defines the set of elements to be exported when SaveTabs is called
  /// </summary>
  /// <remarks>
  ///   If the set is empty the Title is exported. This is equivalent to using
  ///   the <see cref="neTabTypes|TExportElement">expTitle</see> identifier.
  /// </remarks>
  {$ENDREGION}
  TExportElements= set of TExportElement;
  TSetOfStrings = array of string;

  TneTimer = class(TTimer)
  public
    Action: TIntAction;
    Value: string;
  end;

  TneNotificationClass = class(TInterfacedObject, INotification)
  private
    fActions: TIntActions;
    fSender: TObject;
    fValue: string;
    fValueInt: integer;
    fPoint: TPointF;
    fPopupBefore,
    fPopupDefault,
    fPopupAfter: TPopupMenu;
  public
    property Action: TIntActions read fActions write fActions;
    property Value: string read fValue write fValue;
    property ValueInt: Integer read fValueInt write fValueInt;
    property Sender: TObject read fSender write fSender;
    property Point: TPointF read fPoint write fPoint;
    property PopupBefore: TPopupMenu read fPopupBefore write fPopupBefore;
    property PopupDefault: TPopupMenu read fPopupDefault write fPopupDefault;
    property PopupAfter: TPopupMenu read fPopupAfter write fPopupAfter;
  end;

  THistoryClass = class
  private
    fHistoryList: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddHistory(const newStr: string);
    procedure DeleteHistory (const delStr: string);
    function GetLastEntry: string;
  end;

resourcestring
  SCloseTab = 'Close';
  SCloseAllTabs = 'Close All Tabs';
  SCloseOtherTabs = 'Close All Other Tabs';

implementation

uses
	System.SysUtils, FMX.Styles.Objects;

{ THistoryClass }

procedure THistoryClass.AddHistory(const newStr: string);
begin
  if trim(newStr)='' then
    Exit;
  fHistoryList.Add(trim(newStr));
end;

constructor THistoryClass.Create;
begin
  inherited;
  fHistoryList:=TList<string>.Create;
end;

procedure THistoryClass.DeleteHistory(const delStr: string);
var
  i: Integer;
begin
  i:=0;
  while i<=fHistoryList.Count-1 do
  begin
    if fHistoryList.Items[i]=trim(delStr) then
      fHistoryList.Remove(trim(delStr))
    else
      i:=i+1;
  end;
end;

destructor THistoryClass.Destroy;
begin
  inherited;
  fHistoryList.Free;
end;

function THistoryClass.GetLastEntry: string;
begin
  if fHistoryList.Count>0 then
    result:=fHistoryList.Items[fHistoryList.Count-1]
  else
    result:='';
end;


end.
