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
// Unit Name: neTabControl
//
//
//
//***************************************************************

unit neTabControl;

interface

uses
  System.Classes, FMX.Types, FMX.Controls, FMX.Layouts,
  FMX.Styles.Objects, FMX.StdCtrls,FMX.TabControl, System.Generics.Collections,
  FMX.Forms, FMX.Graphics, FMX.Objects, System.UITypes, Model.Provider,
  Model.Interf, Model.Subscriber, Model.IntActions,
  FMX.Menus,
  neTabTypes, neTabGeneralUtils, neTabItem;

const
  MajorVersion = '1';
  MinorVersion = '1';
  BugVersion = '1';


//***************************************************************
//
// Version History
//
//
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
  TneTabPosition = (Top, Left, Right, Bottom);
  TneSibebarControlPosition = (sbLeft, sbRight);

  TOnBeforeAddItem = procedure (var newItem:TneTabItem; var ContinueAdd: Boolean)
                    of object;
  TOnAfterAddItem = procedure (var newItem: TneTabItem) of object;

  TOnBeforeDeleteItem = procedure (const item: TneTabItem; var ContinueDelete: Boolean)
                    of object;
  TOnAfterDeleteItem = procedure (var item: TneTabItem) of object;

  TOnBeforePopupMenu = procedure (
                                  const tabItem: TneTabItem;
                                  var popupMenu: TPopupMenu;
                                  var canContinue: Boolean) of object;

  TOnBeforeAddSibarControl = procedure (var newControl:Tcontrol; var ContinueAdd: Boolean)
                    of object;
  TOnAfterAddSidebarControl = procedure (var newItem: TControl) of object;

  TOnBeforeDeleteSidebarControl = procedure (const item: TControl; var ContinueDelete: Boolean)
                    of object;
  TOnAfterDeleteSidebarControl = procedure (var item: TControl) of object;

  /// <summary>
  ///   This is the tab control
  /// </summary>
  /// <remarks>
  ///   Note that from version 1.0.0 onwards, neTabControl is a StyledControl
  ///   and not a TabControl as it was in previous versions
  /// </remarks>
  [ComponentPlatforms(pidWin32 or pidWin64 or pidOSX32)]
  TneTabControl = class(TStyledControl)
  private
    fMainLayout,
    fLeftLayout,
    fRightLayout: TLayout;
    fMainGridLayout: TGridPanelLayout;
    fTabBar: TTabControl;
    fMainControl: TTabControl;

    fInternalTimer: TneTimer;

    //properties
    fTabPosition: TneTabPosition;
    fTabHeight: Integer;
    fCloseImageNormal,
    fCloseImageHover,
    fCloseImagePressed: TBitmap;
    fActiveTab: TneTabItem;
    fActiveTag: string;
    fCloseTabText,
    fCloseAllTabsText,
    fCloseAllOtherTabsText: string;

    //Internal
    /// <summary>
    ///   Holds the pairs Tag: TneTabItem
    /// </summary>
    fTabsDictionary: TDictionary<string, TneTabItem>;
    /// <summary>
    ///   Holds the pairs Tag: TFrame
    /// </summary>
    fFramesDictionary: TDictionary<string, TFrame>;
    /// <summary>
    ///   Holds the pair Tag: TTabItem, which is added in the Main tab control
    /// </summary>
    fMainTabsDictionary: TDictionary<string, TTabItem>;

    fSubscriber: ISubscriber;

    {$REGION 'Keeps the tags the user is visiting. Used to revert to the last tab when a tab item is deleted'}
    /// <summary>
    ///   Keeps the tags the user is visiting. Used to revert to the last tab
    ///   when a tab item is deleted
    /// </summary>
    {$ENDREGION}
    fHistoryList: THistoryClass;

    fLeftSidebarControls,
    fRightSidebarControls: TList<TControl>;

    //Events
    fOnBeforeAddItem: TOnBeforeAddItem;
    fOnAfterAddItem: TOnAfterAddItem;
    fOnChange: TNotifyEvent;
    fOnBeforeDeleteItem: TOnBeforeDeleteItem;
    fOnAfterDeleteItem: TOnAfterDeleteItem;
    fOnBeforePopupMenu: TOnBeforePopupMenu;

    fOnBeforeAddSidebarControl: TOnBeforeAddSibarControl;
    fOnAfterAddSidebarControl: TOnAfterAddSidebarControl;
    fOnBeforeDeleteSidebarControl: TOnBeforeDeleteSidebarControl;
    fOnAfterDeleteSidebarControl: TOnAfterDeleteSidebarControl;

    //Foundation Fields
    fVersion: string;

    //Procedures/Function
    procedure SetTabPosition (const newPos: TneTabPosition);
    /// <summary>
    ///   Updates the positon of the controls dpending on the TnePosition (Top,
    ///   Left, Right, Bottom)
    ///   It, also, updates the layout widths
    /// </summary>
    procedure UpdatePosition;
    function GetTabCount: Integer;

    procedure SetTabHeight (const newHeight: Integer);

    procedure SetActiveTab(const newTab: TneTabItem);
    procedure SetActiveTag(const newTag: string);

    procedure ShowPopupMenu (const notificationClass: INotification);

    //Private Events
    procedure OnChangeTab(Sender: TObject);
    procedure OnInternalTimer(Sender: TObject);
    procedure OnCloseAllTabs(Sender: TObject);
    procedure OnCloseOtherTabs(Sender: TObject);
    procedure OnSelectTabInPopupMenu(Sender: TObject);
    procedure OnMouseEnterToMainControl(Sender: TObject);

    //Foundation procedures/functions
    function GetVersion:string;
    procedure UpdateFromProvider(const notificationClass: INotification);
  protected
    // Protected declarations
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    ///   Adds a tab in the TabControl with the default TabItem and no Frame
    /// </summary>
    procedure AddTab(const Tag: string); overload;
    /// <summary>
    ///   Adds a tab in the TabControl without a Frame
    /// </summary>
    procedure AddTab(const Tag: string; var newItem: TneTabItem); overload;
    procedure AddTab(const Tag: string; var newItem: TneTabItem;
        var newFrame: TFrame); overload;

    /// <summary>
    ///   Inserts a tab in the TabControl with the default TabItem and no Frame
    ///  in the tabIndex position
    /// </summary>
    procedure InsertTab(const Tag: string; const tabIndex: integer); overload;
    /// <summary>
    ///   Inserts a tab in the TabControl without a Frame  in the
    ///  tabIndex positiob
    /// </summary>
    procedure InsertTab(const Tag: string; const tabIndex: integer;
      var newItem: TneTabItem); overload;
    procedure InsertTab(const Tag: string; const tabIndex: integer;
      var newItem: TneTabItem;
        var newFrame: TFrame); overload;
    /// <summary>
    ///   Deletes the tab with the specific tag
    /// </summary>
    /// <param name="Tag">
    ///   The tag of the tab to be deleted
    /// </param>
    /// <param name="forceDelete">
    ///   If true, the tab is deleted even if the <see cref="neTabControl|TneTabItem.CanClose">
    ///   CanClose</see> property is false
    /// </param>
    procedure DeleteTab(const Tag: string; const forceDelete: Boolean=False);

    /// <summary>
    ///   Generates a list of TneTabItems sorted based on the index, ie. the
    ///   way they appear in the tabcontrol
    /// </summary>
    procedure GetTabItemsList(var sortedTabs: TList<TneTabItem>);
    /// <summary>
    ///   Returns the list of the tabs in the order they appear in the tab
    ///   control
    /// </summary>
    procedure GetTagList(var tagList: TStringList);
    function GetTag(const tabIndex: Integer): string;
    function GetTab(const tag: string): TneTabItem;
    function GetControl (const tag: string): TControl;
    {$REGION 'Refreshes the control of the tab. Must be called everytime the ControlToShow is altered'}
    /// <summary>
    ///   Refreshes the control of the tab. Must be called everytime the <see cref="neTabItem|TneTabItem.ControlToShow">
    ///   ControlToShow</see> is altered
    /// </summary>
    {$ENDREGION}
    procedure RefreshControl(const tag: string); overload;
    {$REGION 'Refreshes the control and adjusts the tab height.'}
    /// <summary>
    ///   Refreshes the control and adjusts the tab height.
    /// </summary>
    /// <remarks>
    ///   This is called after any changes to style of the form with the
    ///   controller. It is a awkward way to fix the issue of different tab
    ///   heights. The heights are hard-coded.
    /// </remarks>
    {$ENDREGION}
    procedure RefreshTabHeight(const StyleFileName: string); overload;

    {$REGION 'Adds a control to one of the two sidebars'}
    /// <summary>
    ///   Adds a control to one of the two sidebars
    /// </summary>
    /// <param name="newControl">
    ///   The controls to add
    /// </param>
    /// <param name="position">
    ///   Defines in which sidebar the control will be added (See <see cref="neTabControl|tneSibebarControlPosition">
    ///   TneSidebarControlPosition</see>)
    /// </param>
    {$ENDREGION}
    procedure AddSidebarControl (const newControl: TControl; const position: tneSibebarControlPosition);
    function GetSidebarControl (const controlindex: Integer; const position: tneSibebarControlPosition): TControl;
    procedure DeleteSidebarControl (const controlIndex: Integer; const position: tneSibebarControlPosition);
    procedure ToggleVisible(const visibleValue: Boolean; const position: tneSibebarControlPosition);
    function GetSidebarControlCount (const position: tneSibebarControlPosition): Integer;
    /// <summary>
    ///   Returns the number of the tabs
    /// </summary>
    property TabCount: integer read GetTabCount;

    {$REGION 'Saves the open tabs to a list of strings.'}
    /// <summary>
    ///   Saves the open tabs to a list of strings.
    /// </summary>
    /// <param name="exportElements">
    ///   Defines which elements will be used in the export data. See <see cref="neTabTypes|TExportElements" />
    /// </param>
    /// <param name="excludeTabs">
    ///   Defines which tabs (by tag) will be excluded from the save list
    /// </param>
    /// <param name="savedTabs">
    ///   The list of saved tabs
    /// </param>
    /// <remarks>
    ///   <para>
    ///     <b>Export Structure</b>
    ///   </para>
    ///   <para>
    ///     Tabs are exported as they appear in the tab.
    ///   </para>
    ///   <list type="bullet">
    ///     <item>
    ///       [tag] = the tag of the neTabItem <br /><br />
    ///     </item>
    ///     <item>
    ///       [tagString] = the TagString property of the neTabItem <br /><br />
    ///     </item>
    ///     <item>
    ///       [tagFloat] = the TagFloat property of the neTabItem <br />
    ///     </item>
    ///   </list>
    /// </remarks>
    {$ENDREGION}
    procedure SaveTabs(const exportElements: TExportElements;
      const excludeTabs: TSetOfStrings; var savedTabs: TStringList);

  published
    /// <summary>
    ///   Defines the position of the tabs
    /// </summary>
    property TabPosition: TneTabPosition read fTabPosition write SetTabPosition
                default TneTabPosition.Top;
    /// <summary>
    ///   Defines the height of the tab item
    /// </summary>
    property TabHeight: integer read fTabHeight write SetTabHeight default 30;
    property CloseImageNormal: TBitmap read fCloseImageNormal write fCloseImageNormal;
    property CloseImageHover: TBitmap read fCloseImageHover write fCloseImageHover;
    property CloseImagePressed: TBitmap read fCloseImagePressed write fCloseImagePressed;

    property ActiveTab: TneTabItem read fActiveTab write SetActiveTab;
    property ActiveTag: string read fActiveTag write SetActiveTag;

    property CloseTabText: string read fCloseTabText write fCloseTabText;
    property CloseAllTabsText: string read fCloseAllTabsText write fCloseAllTabsText;
    property CloseAllOtherTabsText: string read fCloseAllOtherTabsText
        write fCloseAllOtherTabsText;

    property Align;
    property Anchors;
    property CanParentFocus;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DisableFocusEffect default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property TabOrder;
    property Visible default True;
    property Width;
    property Size;
    property OnApplyStyleLookup;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnKeyDown;
    property OnKeyUp;
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;


    //Events
    /// <summary>
    ///   This event is called before a new Item is added
    /// </summary>
    /// <remarks>
    ///   If <see cref="neTabControl|TOnBeforeAddItem">ContitueAdd</see> is
    ///   true the item will be added.
    /// </remarks>
    property OnBeforeAddItem: TOnBeforeAddItem read fOnBeforeAddItem
          write fOnBeforeAddItem;
    /// <summary>
    ///   This event is called after a new Item is added
    /// </summary>
    property OnAfterAddItem: TOnAfterAddItem read fOnAfterAddItem
          write fOnAfterAddItem;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnBeforeDeleteItem: TOnBeforeDeleteItem read fOnBeforeDeleteItem
          write fOnBeforeDeleteItem;
    property OnAfterDeleteItem: TOnAfterDeleteItem read fOnAfterDeleteItem
          write fOnAfterDeleteItem;
    {$REGION 'This event is fired just before the popup menu is shown.'}
    /// <summary>
    ///   This event is fired just before the popup menu is shown.
    /// </summary>
    {$ENDREGION}
    property OnBeforePopupMenu: TOnBeforePopupMenu read fOnBeforePopupMenu
          write fOnBeforePopupMenu;

    property OnBeforeAddSidebarControl: TOnBeforeAddSibarControl
          read fOnBeforeAddSidebarControl write fOnBeforeAddSidebarControl;
    property OnAfterAddSidebarControl: TOnAfterAddSidebarControl
          read fOnAfterAddSidebarControl write fOnAfterAddSidebarControl;
    property OnBeforeDeleteSidebarControl: TOnBeforeDeleteSidebarControl
          read fOnBeforeDeleteSidebarControl write fOnBeforeDeleteSidebarControl;
    property OnAfterDeleteSidebarControl: TOnAfterDeleteSidebarControl
          read fOnAfterDeleteSidebarControl write fOnAfterDeleteSidebarControl;

    //Foundation properties
    property Version: string read GetVersion;
  end;

procedure Register;

function GetTabControl (const aClass: TClass): TneTabControl;

implementation

uses
  System.Math, System.Types, FMX.Dialogs, System.Generics.Defaults, System.SysUtils,
  System.StrUtils;

procedure Register;
begin
  RegisterComponents('NusEnvision', [TneTabControl]);
end;

function GetTabControl (const aClass: TClass): TneTabControl;
begin
  result:=nil;
end;

{ TneTabControl }

procedure TneTabControl.AddTab(const Tag: string);
var
  newTab: TneTabItem;
begin
  if (Trim(Tag)='') or (fTabsDictionary.ContainsKey(Tag)) then
    Exit;
  newTab:=TneTabItem.Create(self);
  newTab.Text:='Tab '+(fTabBar.TabCount+1).ToString;
  AddTab(Tag, newTab);
end;

procedure TneTabControl.AddTab(const Tag: string; var newItem: TneTabItem);
var
  newFrame: TFrame;
begin
  if (Trim(Tag)='') or (fTabsDictionary.ContainsKey(Tag)) or
    (not Assigned(newItem)) then
    Exit;
  newframe:=TFrame.Create(self);
  AddTab(Tag, newItem, newFrame);
end;

procedure TneTabControl.AddSidebarControl(const newControl: TControl;
  const position: tneSibebarControlPosition);
var
  continueAdd: Boolean;
  tmpControl: TControl;
begin
  if not Assigned(newControl) then
    Exit;
  continueAdd:=true;
  tmpControl:=newControl;
  if Assigned(fOnBeforeAddSidebarControl) then
    fOnBeforeAddSidebarControl(tmpControl, continueAdd);
  if not continueAdd then
    Exit;
  case position of
    sbLeft: begin
              newControl.Align:=TAlignLayout.Left;
              newControl.Parent:=fLeftLayout;
              fLeftSidebarControls.Add(newControl);
            end;
    sbRight: begin
              newControl.Align:=TAlignLayout.Right;
              newControl.Parent:=fRightLayout;
              fRightSidebarControls.Add(newControl);
              end;
  end;
  UpdatePosition;
  if Assigned(fOnAfterAddSidebarControl) then
    fOnAfterAddSidebarControl(tmpControl);
end;

procedure TneTabControl.AddTab(const Tag: string; var newItem: TneTabItem;
  var newFrame: TFrame);
var
  mainTab: TTabItem;
  continueAdding: Boolean;
  trimTag: string;
begin
  trimTag:=Trim(Tag);
  if (trimTag='') or (fTabsDictionary.ContainsKey(Tag)) or
    (not Assigned(newItem)) or (not Assigned(newFrame)) then
    Exit;

  continueAdding:=True;
  if Assigned(fOnBeforeAddItem) then
    fOnBeforeAddItem(newItem, continueAdding);

  if not continueAdding then
    Exit;

  newItem.TabTag:=trimTag;
  newItem.CloseImageNormal:=fCloseImageNormal;
  newItem.CloseImageHover:=fCloseImageHover;
  newItem.CloseImagePressed:=fCloseImagePressed;

  newFrame.TagString:=trimTag;

  fTabsDictionary.Add(trimTag, newItem);
  fFramesDictionary.Add(trimTag, newFrame);

  newItem.Parent:=fTabBar;
  fTabBar.AddObject(newItem);

  newItem.TabWidth:=newItem.MinTabWidth;

  newItem.Provider.Subscribe(fSubscriber);

  mainTab:=TTabItem.Create(self);
  mainTab.Parent:=fMainControl;
  mainTab.Height:=fTabHeight;

  newFrame.Parent:=mainTab;
  newFrame.OnMouseEnter:=OnMouseEnterToMainControl;

  fMainControl.AddObject(mainTab);
  fMainTabsDictionary.Add(trimTag, mainTab);

////////////
///  If you add a tabitem in the native tabcontrol, delete it and then
///  add a new one, the control doesn't fire the OnChange event
///  This is a fix for this situation
///  See SetActiveTag
///////////
  if fTabBar.TabCount=1 then
        SetActiveTag(GetTag(0));

  if Assigned(fOnAfterAddItem) then
    fOnAfterAddItem(newItem);

end;


constructor TneTabControl.Create(AOwner: TComponent);
begin
  inherited;
  fTabHeight:=30;
  fTabPosition:=TneTabPosition.Top;

  fCloseImageNormal:=TBitmap.Create;
  fCloseImageHover:=TBitmap.Create;
  fCloseImagePressed:=TBitmap.Create;

  fCloseTabText:=SCloseTab;
  fCloseAllTabsText:=SCloseAllTabs;
  fCloseAllOtherTabsText:=SCloseOtherTabs;

  fMainGridLayout:=TGridPanelLayout.Create(Self);
  fMainGridLayout.Align:=TAlignLayout.Client;
  fMainGridLayout.Parent:=self;
  fMainGridLayout.Stored:=false;
  fMainGridLayout.ColumnCollection.Clear;

  fMainGridLayout.ColumnCollection.Add.SizeStyle:=TGridPanelLayout.TSizeStyle.Absolute;
  fMainGridLayout.ColumnCollection.Items[0].Value:=TabHeight;
  fMainGridLayout.ColumnCollection.Add.SizeStyle:=TGridPanelLayout.TSizeStyle.Percent;
  fMainGridLayout.ColumnCollection.Items[1].Value:=100;
  fMainGridLayout.ColumnCollection.Add.SizeStyle:=TGridPanelLayout.TSizeStyle.Absolute;
  fMainGridLayout.ColumnCollection.Items[2].Value:=TabHeight;

  fMainGridLayout.RowCollection.Clear;
  fMainGridLayout.RowCollection.Add.SizeStyle:=TGridPanelLayout.TSizeStyle.Absolute;
  fMainGridLayout.RowCollection.Items[0].Value:=TabHeight;
  fMainGridLayout.RowCollection.Add.SizeStyle:=TGridPanelLayout.TSizeStyle.Percent;
  fMainGridLayout.RowCollection.Items[1].Value:=100;
  fMainGridLayout.RowCollection.Add.SizeStyle:=TGridPanelLayout.TSizeStyle.Absolute;
  fMainGridLayout.RowCollection.Items[2].Value:=TabHeight;

  fMainLayout:=TLayout.Create(self);
  fMainLayout.Stored:=false;
  fMainLayout.Align:=TAlignLayout.Contents;
  fMainLayout.Parent:=fMainGridLayout;
  fMainLayout.RotationCenter.X:=0;
  fMainLayout.RotationCenter.Y:=0;
  with fMainGridLayout.ControlCollection.Items[0] do
  begin
    Column:=0;
    ColumnSpan:=3;
    Row:=0;
    RowSpan:=1;
  end;

  //TneTabPosition is Top by default
  fLeftLayout:=TLayout.Create(self);
  fLeftLayout.Stored:=false;
  fLeftLayout.Parent:=fMainLayout;
  fLeftLayout.Align:=TAlignLayout.Left;
  fLeftLayout.Width:=0;

  fTabBar:=TTabControl.Create(self);
  fTabBar.Parent:=fMainLayout;
  fTabBar.Stored:=False;
  fTabBar.Align:=TAlignLayout.Client;
  fTabBar.TabPosition:=TTabPosition.Top;
  fTabBar.OnChange:=OnChangeTab;
  fTabBar.TabHeight:=TabHeight;

  fRightLayout:=TLayout.Create(self);
  fRightLayout.Stored:=False;
  fRightLayout.Parent:=fMainLayout;
  fRightLayout.Align:=TAlignLayout.Right;
  fRightLayout.Width:=0;

  fMainControl:=TTabControl.Create(self);
  fMainControl.Anchors:=[TAnchorKind.akLeft, TAnchorKind.akTop,
                            TAnchorKind.akRight, TAnchorKind.akBottom];
  fMainControl.Stored:=false;
  fMainControl.Parent:=fMainGridLayout;
  fMainControl.Align:=TAlignLayout.Client;
  fMainControl.TabPosition:=TTabPosition.None;
  with fMainGridLayout.ControlCollection.Items[1] do
  begin
    Column:=0;
    ColumnSpan:=3;
    Row:=1;
    RowSpan:=2;
  end;

  self.Anchors:=[TAnchorKind.akLeft, TAnchorKind.akTop];
  self.Height:=300;
  self.Width:=300;
  self.Align:=TAlignLayout.None;

  fInternalTimer:=TneTimer.Create(self);
  fInternalTimer.Interval:=10;
  fInternalTimer.OnTimer:=OnInternalTimer;
  fInternalTimer.Enabled:=false;

  fTabsDictionary:=TDictionary<string, TneTabItem>.Create;
  fFramesDictionary:=TDictionary<string, TFrame>.Create;
  fMainTabsDictionary:=TDictionary<string, TTabItem>.Create;

  fSubscriber:=SubscriberClass;
  fSubscriber.SetUpdateSubscriberMethod(UpdateFromProvider);

  fHistoryList:=THistoryClass.Create;
  fLeftSidebarControls:=TList<TControl>.Create;
  fRightSidebarControls:=TList<TControl>.Create;

end;

procedure TneTabControl.DeleteSidebarControl(const controlIndex: Integer;
  const position: tneSibebarControlPosition);
var
  tmpControl: TControl;
  tmpList: TList<TControl>;
  continueDel: Boolean;
begin
  case position of
    sbLeft: tmpList:=fLeftSidebarControls;
    sbRight: tmpList:=fRightSidebarControls;
  else
    tmpList:=fLeftSidebarControls;
  end;

  if (controlindex>=0) and
                  (controlindex<=tmpList.Count-1) then
  begin
    tmpControl:=tmpList.Items[controlIndex];

    continueDel:=true;
    if Assigned(fOnBeforeDeleteSidebarControl) then
      fOnBeforeDeleteSidebarControl(tmpControl, continueDel);
    if not continueDel then
      Exit;
    tmpList.Remove(tmpControl);
    if Assigned(fOnAfterDeleteSidebarControl) then
      fOnAfterDeleteSidebarControl(tmpControl)
    else
      tmpControl.Free;
    UpdatePosition;
  end;
end;

procedure TneTabControl.DeleteTab(const Tag: string;
  const forceDelete: Boolean);
var
  continueDelete: Boolean;
  delItem: TneTabItem;
  delFrame: TFrame;
  lastTag: string;
begin
  if (Trim(Tag)='') or (not fTabsDictionary.ContainsKey(Trim(Tag))) then
    Exit;

  delItem:=fTabsDictionary.Items[Trim(Tag)];
  if not Assigned(delItem) then
    Exit;
  continueDelete:=true;

  if Assigned(fOnBeforeDeleteItem) then
    fOnBeforeDeleteItem(delItem, continueDelete);

  if (not continueDelete) or ((not forceDelete) and (not delItem.CanClose)) then
    Exit;

  fTabBar.Delete(delItem.Index);
  fTabsDictionary.Remove(Trim(Tag));

  delFrame:=fFramesDictionary.Items[Trim(Tag)];
  delFrame.Parent:=nil;

  fMainControl.RemoveObject(delFrame);
  fMainTabsDictionary.Remove(Trim(Tag));

  if Assigned(fOnAfterDeleteItem) then
    fOnAfterDeleteItem(delItem);

  fFramesDictionary.Remove(Trim(tag));

  fHistoryList.DeleteHistory(Trim(tag));

  lastTag:=fHistoryList.GetLastEntry;

  if (lastTag='') and (fTabsDictionary.Count>0) then
    SetActiveTag(GetTag(0))
  else
    SetActiveTag(lastTag);

end;

destructor TneTabControl.Destroy;
begin
  fTabsDictionary.Free;
  fFramesDictionary.Free;
  fMainTabsDictionary.Free;
  fCloseImageNormal.Free;
  fCloseImageHover.Free;
  fCloseImagePressed.Free;
  fInternalTimer.Free;
  fHistoryList.Free;
  fLeftSidebarControls.Free;
  fRightSidebarControls.Free;
  inherited;
end;

function TneTabControl.GetControl(const tag: string): TControl;
begin
  if fTabsDictionary.ContainsKey(Trim(tag)) then
    result:=fTabsDictionary.Items[trim(tag)].ControlToShow
  else
    result:=nil;
end;

function TneTabControl.GetSideBarControl(const controlindex: Integer;
  const position: tneSibebarControlPosition): TControl;
begin
  result:=nil;
  case position of
    sbLeft: if (controlindex>=0) and
                (controlindex<=fLeftSidebarControls.Count-1) then
                  result:=fLeftSidebarControls.Items[controlindex];
    sbRight: if (controlindex>=0) and
                (controlindex<=fRightSidebarControls.Count-1) then
                  result:=fRightSidebarControls.Items[controlindex];
  end;
end;

function TneTabControl.GetSidebarControlCount(
  const position: tneSibebarControlPosition): Integer;
begin
  result:=0;
  case position of
    sbLeft: result:=fLeftSidebarControls.Count;
    sbRight: result:=fRightSidebarControls.Count;
  end;
end;

procedure TneTabControl.GetTabItemsList(var sortedTabs: TList<TneTabItem>);
var
  tmpItem: TneTabItem;
  tmpComparison: TComparison<TneTabItem>;
begin

  tmpComparison:=
    function (const Left, Right: TneTabItem): integer
    begin
      result:=TComparer<integer>.Default.Compare(left.Index, right.Index);
    end;

  for tmpItem in fTabsDictionary.Values do
    sortedTabs.Add(tmpItem);
  sortedTabs.Sort(TComparer<TneTabItem>.Construct(tmpComparison));

end;

function TneTabControl.GetTab(const tag: string): TneTabItem;
begin
  if fTabsDictionary.ContainsKey(Trim(tag)) then
    result:=fTabsDictionary.Items[Trim(tag)]
  else
    result:=nil;
end;

function TneTabControl.GetTabCount: Integer;
begin
  result:=fTabsDictionary.Count;
end;

function TneTabControl.GetTag(const tabIndex: Integer): string;
var
  tmpList: TList<TneTabItem>;
begin
  if (tabIndex<-1) or (tabIndex>fTabsDictionary.Count-1) then
    exit;
  tmpList:=Tlist<TneTabItem>.Create;
  GetTabItemsList(tmpList);
  result:=tmpList.Items[tabIndex].TabTag;
  tmpList.Free;
end;

procedure TneTabControl.GetTagList(var tagList: TStringList);
var
  tmpList: TList<TneTabItem>;
  i: integer;
begin
  if not Assigned(tagList) then
    Exit;
  tmpList:=TList<TneTabItem>.Create;
  GetTabItemsList(tmpList);
  for i:=0 to tmpList.Count-1 do
    tagList.Add(tmpList.Items[i].TabTag);
  tmpList.Free;
end;

function TneTabControl.
GetVersion: string;
begin
  result:=MajorVersion+'.'+MinorVersion+'.'+BugVersion;
end;

procedure TneTabControl.InsertTab(const Tag: string; const tabIndex: integer);
var
  insTab: TneTabItem;
  tmpTag: string;
begin
  tmpTag:=Trim(tag);
  if tmptag='' then
    exit;
  if (tabIndex<0) or (tabIndex>TabCount) then
    exit;
  AddTab(tmpTag);
  insTab:=GetTab(tmpTag);
  if Assigned(insTab) then
    insTab.Index:=tabIndex;
end;

procedure TneTabControl.InsertTab(const Tag: string; const tabIndex: integer;
  var newItem: TneTabItem);
var
  insTab: TneTabItem;
  tmpTag: string;
begin
  tmpTag:=Trim(tag);
  if tmptag='' then
    exit;
  if (tabIndex<0) or (tabIndex>TabCount) then
    exit;
  AddTab(tmpTag, newItem);
  insTab:=GetTab(tmpTag);
  if Assigned(insTab) then
    insTab.Index:=tabIndex;
end;

procedure TneTabControl.InsertTab(const Tag: string; const tabIndex: integer;
  var newItem: TneTabItem; var newFrame: TFrame);
var
  insTab: TneTabItem;
  tmpTag: string;
begin
  tmpTag:=Trim(tag);
  if tmptag='' then
    exit;
  if (tabIndex<0) or (tabIndex>TabCount) then
    exit;
  AddTab(tmpTag, newItem, newFrame);
  insTab:=GetTab(tmpTag);
  if Assigned(insTab) then
    insTab.Index:=tabIndex;
end;

procedure TneTabControl.OnChangeTab(Sender: TObject);
var
  tmpActiveTag: string;
begin
  tmpActiveTag:=((Sender as TTabControl).ActiveTab as TneTabItem).TabTag;
  if fMainTabsDictionary.ContainsKey(tmpActiveTag) and
       fFramesDictionary.ContainsKey(tmpActiveTag) then
  begin
    fMainControl.ActiveTab:=fMainTabsDictionary.Items[tmpActiveTag];
    fHistoryList.AddHistory(tmpActiveTag);
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;

procedure TneTabControl.OnCloseAllTabs(Sender: TObject);
begin
  while self.tabcount>0 do
    Self.DeleteTab(self.GetTag(0));
end;

procedure TneTabControl.OnCloseOtherTabs(Sender: TObject);
var
  i: Integer;
begin
  i:=0;
  if not (Sender is TMenuItem) then
    Exit;
  while self.tabcount>1 do
  begin
    if (Sender as TMenuItem).TagString=Self.GetTag(i) then
      i:=i+1;
    Self.DeleteTab(self.GetTag(i));
  end;
  self.ActiveTag:=Self.GetTag(0);
end;

procedure TneTabControl.OnInternalTimer(Sender: TObject);
begin
  fInternalTimer.Enabled:=false;
  case fInternalTimer.Action of
    intactDeleteTab: DeleteTab(fInternalTimer.Value);
  end;
end;

procedure TneTabControl.OnMouseEnterToMainControl(Sender: TObject);
var
  tmpList: TList<TneTabItem>;
  i: Integer;
begin
  tmpList:=TList<TneTabItem>.Create;
  GetTabItemsList(tmpList);
  for I := 0 to tmpList.Count-1 do
    tmpList.Items[i].RefreshControl;
  tmpList.Free;
end;

procedure TneTabControl.OnSelectTabInPopupMenu(Sender: TObject);
begin
  if not (Sender is TMenuItem) then
    Exit;
  self.ActiveTag:=(Sender as TMenuItem).TagString;
end;

procedure TneTabControl.RefreshControl(const tag: string);
begin
 if fTabsDictionary.ContainsKey(Trim(tag)) then
   if Assigned(GetTab(Trim(tag))) then
    GetTab(Trim(tag)).RefreshControl;
end;

procedure TneTabControl.RefreshTabHeight(const StyleFileName: string);
var
  str: string;
begin
///////////////////
///  This is a very ugly implementation
///  Need to figure out how to get the height of the tabitem according
///  to the style file
///
//////////////////

{TODO -oOwner -cCategory : Get the height of the tabitem according to the style}

  if (Trim(StyleFileName)='') or (not (UpperCase(SplitString(StyleFileName,'.')[1])='STYLE')) then
    TabHeight:=30
  else
  begin
    str:=UpperCase(SplitString(StyleFileName,'.')[0]);
    if str=UpperCase('Air') then TabHeight:=30;
    if str=UpperCase('Amakrits') then TabHeight:=34;
    if str=UpperCase('AquaGraphite') then TabHeight:=34;
    if str=UpperCase('Blend') then TabHeight:=34;
    if str=UpperCase('Dark') then TabHeight:=34;
    if str=UpperCase('GoldenGraphite') then TabHeight:=34;
    if str=UpperCase('Light') then TabHeight:=32;
    if str=UpperCase('MetropolisUIBlack_touch') then TabHeight:=50;
    if str=UpperCase('MetropolisUIBlack') then TabHeight:=30;
    if str=UpperCase('MetropolisUIBlue_touch') then TabHeight:=50;
    if str=UpperCase('MetropolisUIBlue') then TabHeight:=30;
    if str=UpperCase('MetropolisUIDark_touch') then TabHeight:=50;
    if str=UpperCase('MetropolisUIDark') then TabHeight:=30;
    if str=UpperCase('MetropolisUIGreen_touch') then TabHeight:=50;
    if str=UpperCase('MetropolisUIGreen') then TabHeight:=30;
    if str=UpperCase('RubyGraphite') then TabHeight:=34;
    if str=UpperCase('Transparent') then TabHeight:=28;
    if str=UpperCase('Win10Modern') then TabHeight:=30;
    if str=UpperCase('Win10ModernBlue') then TabHeight:=34;
    if str=UpperCase('Win10ModernDark') then TabHeight:=34;
  end;

end;

procedure TneTabControl.SaveTabs(const exportElements: TExportElements;
  const excludeTabs: TSetOfStrings; var savedTabs: TStringList);
var
  tmpString: string;
  i: Integer;
  tmpItem: TneTabItem;
  tmpTabList: TList<TneTabItem>;
  tmpExportElements: TExportElements;

  function IsTagInExludeTabs(const tag: string): boolean;
  var
    i: Integer;
  begin
    result:=false;
    for I := 0 to Length(excludeTabs)-1 do
     if trim(tag)=trim(excludeTabs[i]) then
     begin
       result:=true;
       exit;
     end;
  end;


begin
  ///  Export Structure (tabs are exported as they appear in the tab)
  ///  The order of the exported tags is important.
  ///  [tag] = the tag of the netabitem
  ///  [tagString] = the tagString property of the neTabItem if selected in exportElements
  ///  [tagFloat] = the tagFloat property of the neTabitem if selected in exportElements

  if not Assigned(savedTabs) then
    Exit;

  savedTabs.Clear;
  if exportElements=[] then
    tmpExportElements:=[expTitle]
  else
    tmpExportElements:=exportElements;

  tmpTabList:=TList<TneTabItem>.Create;
  GetTabItemsList(tmpTabList);

  for I := 0 to tmpTabList.Count-1 do
  begin
    tmpString:='';
    tmpItem:=tmpTabList.Items[i];
    if not IsTagInExludeTabs(tmpItem.TabTag) then
    begin
      tmpString:=tmpItem.TabTag;
      if expTitle in tmpExportElements then
        tmpString:=tmpString+','+tmpitem.Text;
      if expTagString in tmpExportElements then
        tmpString:=tmpString+', '+tmpitem.TagString;
      if expTagFloat in tmpExportElements then
        tmpString:=tmpString+', '+tmpitem.TagFloat.ToString;
      savedTabs.Add(tmpString);
    end
  end;

  tmpTabList.Free;

end;

procedure TneTabControl.SetActiveTab(const newTab: TneTabItem);
begin
  if (not Assigned(newTab)) or
    (not (fTabsDictionary.ContainsKey(Trim(newTab.TabTag)))) then
    Exit;

  fTabBar.ActiveTab:=newTab;
  fActiveTag:=newTab.TabTag;
end;

procedure TneTabControl.SetActiveTag(const newTag: string);

begin
  if fTabsDictionary.ContainsKey(Trim(newTag)) then
  begin
    ActiveTab:=fTabsDictionary.Items[Trim(newTag)];
////////////
///  If you add a tabitem in the native tabcontrol, delete it and then
///  add a new one, the control doesn't fire the OnChange event
///  This is a fix for this situation
///  The call comes from AddTab
///////////
    if fTabBar.TabCount=1 then
      OnChangeTab(fTabBar);
  end;
end;

procedure TneTabControl.SetTabHeight(const newHeight: Integer);
begin
  if newHeight=0 then
    fTabHeight:=30
  else
    fTabHeight:=newHeight;
  fMainGridLayout.RowCollection.Items[0].Value:=fTabHeight;
  fMainLayout.Height:=fTabHeight;
  fTabBar.Height:=fTabHeight;
end;

procedure TneTabControl.SetTabPosition(const newPos: TneTabPosition);
begin
  fTabPosition:=newPos;
  UpdatePosition;
end;

procedure TneTabControl.ShowPopupMenu(
  const notificationClass: INotification);
var
  tmpPopup: TPopupMenu;
  tmpMenuItem: TMenuItem;
  tmpNotifClass: TneNotificationClass;
  i: Integer;
  newP: TPointF;
  tmpForm: TForm;
  canCont: boolean;
  tabList: TList<TneTabItem>;
  tmpneItem: TneTabItem;
begin
  if (not Assigned(notificationClass)) then
    Exit;

  tmpNotifClass:=notificationClass as TneNotificationClass;

  if (not Assigned(tmpNotifClass.Sender)) then
    Exit;

  tmpPopup:=TPopupMenu.Create(self);
  tmpPopup.Parent:=self;

  if Assigned(tmpNotifClass.PopupBefore) then
  begin
    for i:=0 to tmpNotifClass.PopupBefore.ItemsCount-1 do
    begin
      tmpMenuItem:=TMenuItem.Create(tmpPopup);
      tmpMenuItem.Text:=tmpNotifClass.PopupBefore.Items[i].Text;
      tmpPopup.AddObject(tmpMenuItem);
    end;
  end;

  if Assigned(tmpNotifClass.PopupDefault) then
  begin
    //Separator
    if tmpPopup.ItemsCount>0 then
    begin
      tmpMenuItem:=TMenuItem.Create(tmpPopup);
      tmpMenuItem.Text:='-';
      tmpPopup.AddObject(tmpMenuItem);
    end;

    for i:=0 to tmpNotifClass.PopupDefault.ItemsCount-1 do
    begin
      tmpMenuItem:=TMenuItem.Create(tmpPopup);
      tmpMenuItem.Text:=tmpNotifClass.PopupDefault.Items[i].Text;
      if tmpMenuItem.Text=SCloseTab then
        tmpMenuItem.Text:=fCloseTabText;
      tmpMenuItem.OnClick:=tmpNotifClass.PopupDefault.Items[i].OnClick;
      tmpPopup.AddObject(tmpMenuItem);
    end;

    if Self.TabCount>1 then
    begin
      //Close All Tabs
      tmpMenuItem:=TMenuItem.Create(tmpPopup);
      tmpMenuItem.Text:=fCloseAllTabsText;
      tmpMenuItem.OnClick:=OnCloseAllTabs;
      tmpPopup.AddObject(tmpMenuItem);

      //Close All Tabs except this one
      tmpMenuItem:=TMenuItem.Create(tmpPopup);
      tmpMenuItem.Text:=fCloseAllOtherTabsText;
      tmpMenuItem.OnClick:=OnCloseOtherTabs;
      tmpMenuItem.TagString:=tmpNotifClass.Value;
      tmpPopup.AddObject(tmpMenuItem);

      //Separator
      tmpMenuItem:=TMenuItem.Create(tmpPopup);
      tmpMenuItem.Text:='-';
      tmpPopup.AddObject(tmpMenuItem);

      //List of open tabs
      tabList:=TList<TneTabItem>.Create;
      GetTabItemsList(tabList);
      for i := 0 to tabList.Count-1 do
      begin
        tmpneItem:=tabList.Items[i];
        tmpMenuItem:=TMenuItem.Create(tmpPopup);
        tmpMenuItem.Text:=tmpneItem.Text;
        tmpMenuItem.TagString:=tmpneItem.TabTag;
        if tmpMenuItem.TagString=(tmpNotifClass.Value) then
          tmpMenuItem.IsChecked:=true;
        tmpMenuItem.OnClick:=OnSelectTabInPopupMenu;
        tmpPopup.AddObject(tmpMenuItem);
      end;
      tabList.Free;
    end;
  end;

  if Assigned(tmpNotifClass.PopupAfter) then
  begin
    //Separator
    if tmpPopup.ItemsCount>0 then
    begin
      tmpMenuItem:=TMenuItem.Create(tmpPopup);
      tmpMenuItem.Text:='-';
      tmpPopup.AddObject(tmpMenuItem);
    end;

    for i:=0 to tmpNotifClass.PopupAfter.ItemsCount-1 do
    begin
      tmpMenuItem:=TMenuItem.Create(tmpPopup);
      tmpMenuItem.text:=tmpNotifClass.PopupAfter.Items[i].Text;
      tmpPopup.AddObject(tmpMenuItem);
    end;
  end;

  canCont:=true;
  if Assigned(fOnBeforePopupMenu) then
    fOnBeforePopupMenu(GetTab(tmpNotifClass.Value), tmpPopup, canCont);

  if not canCont then
    Exit;


  newP := TPointF.Create(tmpNotifClass.Point.X, tmpNotifClass.Point.Y);
  newP := (tmpNotifClass.Sender as TneTabItem).LocalToAbsolute(newP);

  tmpForm:=FindParentForm(self);

  if Assigned(tmpForm) then
  begin
    newP := tmpForm.ClientToScreen(newP);
    tmpPopup.Popup(newP.X, newP.Y);
  end;

end;

procedure TneTabControl.ToggleVisible(const visibleValue: Boolean;
  const position: tneSibebarControlPosition);
begin
  case position of
    sbLeft: fLeftLayout.Visible:=visibleValue;
    sbRight: fRightLayout.Visible:=visibleValue;
  end;
end;

procedure TneTabControl.UpdateFromProvider(
  const notificationClass: INotification);
var
  receivedNotClass: TneNotificationClass;
begin
  if not Assigned(notificationClass) then
    Exit;
  receivedNotClass:=notificationClass as TneNotificationClass;
  if intactDeleteTab in receivedNotClass.Action then
  begin
    fInternalTimer.Action:=intactDeleteTab;
    fInternalTimer.Value:=receivedNotClass.Value;
    fInternalTimer.Enabled:=True;
  end;

  if intactShowPopupMenu in receivedNotClass.Action then
    ShowPopupMenu(receivedNotClass);

  if intactUpdateTabHeight in receivedNotClass.Action then
    self.TabHeight:=receivedNotClass.ValueInt;
end;

procedure TneTabControl.UpdatePosition;
var
  totalWidth,
  i: Integer;
begin
  case fTabPosition of
    TneTabPosition.Top: begin
                      fMainLayout.RotationAngle:=0;
                      fMainLayout.Position.X:=0;
                      fMainLayout.Position.Y:=0;
                      fMainLayout.Align:=TAlignLayout.Client;
                      with fMainGridLayout.ControlCollection.Items[1] do
                      begin
                        Column:=0;
                        ColumnSpan:=3;
                        Row:=1;
                        RowSpan:=2;
                      end;
                      with fMainGridLayout.ControlCollection.Items[0] do
                      begin
                        Column:=0;
                        ColumnSpan:=3;
                        Row:=0;
                        RowSpan:=1;
                      end;
                    end;
    TneTabPosition.Left: begin
                      fMainLayout.RotationAngle:=270;
                      with fMainGridLayout.ControlCollection.Items[0] do
                      begin
                        Column:=0;
                        ColumnSpan:=1;
                        Row:=2;
                        RowSpan:=3;
                      end;
                      with fMainGridLayout.ControlCollection.Items[1] do
                      begin
                        Column:=1;
                        ColumnSpan:=2;
                        Row:=0;
                        RowSpan:=3;
                      end;
                      fMainLayout.Align:=TAlignLayout.None;
                      fMainLayout.Position.Y:=self.Height;
                      fMainLayout.Width:=self.Height;
                    end;

  end;

  //Left sidebar
  totalWidth:=0;

  for I := 0 to fLeftSidebarControls.Count-1 do
    if Assigned(fLeftSidebarControls.Items[i]) then
      totalWidth:=totalWidth+round(fleftSidebarControls.Items[i].Width);
  fLeftLayout.Width:=totalWidth;

  //Right sidebar
  totalWidth:=0;

  for I := 0 to fRightSidebarControls.Count-1 do
    if Assigned(fRightSidebarControls.Items[i]) then
      totalWidth:=totalWidth+round(fRightSidebarControls.Items[i].Width);
  fRightLayout.Width:=totalWidth;

end;

end.
