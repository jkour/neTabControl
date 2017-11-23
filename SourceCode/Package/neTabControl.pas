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
  FMX.Controls, FMX.Layouts,
  FMX.Styles.Objects,FMX.TabControl, System.Generics.Collections,
  FMX.Graphics, System.UITypes, Model.Provider,
  Model.Interf, Model.Subscriber, Model.IntActions,
  FMX.Menus,
  neTabTypes, neTabGeneralUtils, neTabItem, System.Classes, System.Types, FMX.Types,
  FMX.Forms;

//////////////////////////////////////////////////
/// For version info, please see the .inc file ///
//////////////////////////////////////////////////

{$I neTabControlVersionInfo.inc}

type
  TneTabOrientation = (orTop, orLeft, orRight, orBottom);
  TneSibebarControlPosition = (sbLeft, sbRight);
  TneHintType = (Off, Text, Preview, Custom);
  TneDoChangeType = (dcTabBar, dcMainTab);

  TOnBeforeAddItem = procedure (var newItem:TneTabItem; var ContinueAdd: Boolean)
                    of object;
  TOnAfterAddItem = procedure (var newItem: TneTabItem) of object;

  TOnBeforeDeleteItem = procedure (const item: TneTabItem; var ContinueDelete: Boolean)
                    of object;
  TOnAfterDeleteItem = procedure (var item: TneTabItem) of object;

  TOnBeforeCloseAllItems = procedure (var continueClose: Boolean) of object;
  TOnAfterCloseAllItems = procedure of object;

  TOnBeforeCloseOtherItems = procedure (var continueClose: Boolean) of object;
  TOnAfterCloseOtherItems = procedure of object;

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

  TOnCustomHint = procedure (var hintItem: TneTabItem; var HintControl: TControl;
                                var CanContinue:boolean) of object;

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
    fPopup: TPopup;
    fNumofDoMainChangeCalls: Byte;
    //properties
    fTabOrientation: TneTabOrientation;
    fTabPosition: TTabPosition;
    fTabHeight: Integer;
    fCloseImageNormal,
    fCloseImageHover,
    fCloseImagePressed: TBitmap;
    fActiveTab: TneTabItem;
    fActiveTag: string;
    fCloseTabText,
    fCloseAllTabsText,
    fCloseAllOtherTabsText: string;
    fTransitionType: TTabTransition;
    fHintInterval: Cardinal;
    fHintType: TneHintType;
    fCloseTimer: TTimer;
    fTabAnimation: TShowTabanimation;
    fTabAnimationDuration: double;
    fTabAnimationType: TAnimationType;
    fTabAnimationInterpolation: TInterpolationType;

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

    fOnBeforeCloseAllItems: TOnBeforeCloseAllItems;
    fOnAfterCloseAllItems: TOnAfterCloseAllItems;
    fOnBeforeCloseOtherItems: TOnBeforeCloseOtherItems;
    fOnAfterCloseOtherItems: TOnAfterCloseOtherItems;

    fOnBeforeAddSidebarControl: TOnBeforeAddSibarControl;
    fOnAfterAddSidebarControl: TOnAfterAddSidebarControl;
    fOnBeforeDeleteSidebarControl: TOnBeforeDeleteSidebarControl;
    fOnAfterDeleteSidebarControl: TOnAfterDeleteSidebarControl;

    fOnCustomHint: TOnCustomHint;

    //Foundation Fields
    fVersion: string;

    //Procedures/Function
    procedure SetTabOrientation (const newPos: TneTabOrientation);
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

    function GetTabByIndex(const tabIndex: Integer): TneTabItem;

    procedure SetTabPosition(const newTabFormat: TTabPosition);

    procedure DoMainChange(const source: TneDoChangeType);

    {$REGION ''}
    /// <param name="X">
    ///   Comes from MouseMove. Relative to hTabItem
    /// </param>
    /// <param name="Y">
    ///   Comes from MouseMove. Relative to hTabItem
    /// </param>
    {$ENDREGION}
    procedure ShowHint(const hTag: string; const Point: TPointF);

    //Private Events
    procedure OnChangeTab(Sender: TObject);
    procedure OnInternalTimer(Sender: TObject);
    procedure OnCloseAllTabs(Sender: TObject);
    procedure OnCloseOtherTabs(Sender: TObject);
    procedure OnSelectTabInPopupMenu(Sender: TObject);
    procedure OnMouseEnterToMainControl(Sender: TObject);
    procedure OnTabBarChange(Sender: TObject);
    procedure OnMainBarChange(Sender: TObject);

    //Foundation procedures/functions
    function GetVersion:string;
    procedure UpdateFromProvider(const notificationClass: INotification);
    procedure SetHintInterval(const Value: Cardinal);
    procedure OnCloseHintTimer(Sender: TObject);
    function GetTabAnimation: TShowTabanimation;
    procedure setTabAnimation(const Value: TShowTabanimation);
    function GetTabAnimationDuration: double;
    procedure setTabAnimationDuration(const Value: double);
    function GetTabAnimationInterpolation: TInterpolationType;
    function GetTabAnimationType: TAnimationType;
    procedure SetTabAnimationInterpolation(const Value: TInterpolationType);
    procedure setTabAnimationType(const Value: TAnimationType);
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
    {$REGION 'Deletes all the tabs'}
    /// <summary>
    ///   Deletes all the tabs
    /// </summary>
    /// <param name="forceDelete">
    ///   If true, deletes the tabs that have the <see cref="neTabItem|TneTabItem.CanClose">
    ///   CanClose</see> property false
    /// </param>
    {$ENDREGION}
    procedure DeleteAllTabs (const forceDelete: Boolean = False);

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
    function GetFrame(const tag: string): TFrame;
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

    //Public Properties
    property TabCount: integer read GetTabCount;
    property Tabs[const tabIndex: Integer]: TneTabItem read GetTabByIndex;

  published
    {$REGION 'Defines the orientation of the tabs'}
    /// <summary>
    ///   Defines the orientation of the tabs
    /// </summary>
    /// <remarks>
    ///   This is not to be conufsed
    /// </remarks>
    {$ENDREGION}
    property TabOrientation: TneTabOrientation read fTabOrientation write SetTabOrientation
                default TneTabOrientation.orTop;
    property TabPosition: TTabPosition read fTabPosition write SetTabPosition
                default TTabPosition.PlatformDefault;
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
    {$REGION 'Defines the transition effect when tabs change'}
    /// <summary>
    ///   Defines the transition effect when tabs change
    /// </summary>
    /// <value>
    ///   <list type="table">
    ///     <listheader>
    ///       <term>Value</term>
    ///       <description>Description</description>
    ///     </listheader>
    ///     <item>
    ///       <term>None</term>
    ///       <description>No transition effect. This is the default
    ///         effect</description>
    ///     </item>
    ///     <item>
    ///       <term>Slide</term>
    ///       <description>Slide effect</description>
    ///     </item>
    ///   </list>
    /// </value>
    {$ENDREGION}
    property Transition: TTabTransition read fTransitionType write fTransitionType
                  default TTabTransition.None;

    {$REGION 'Defines the time interval for the hint'}
    /// <summary>
    ///   Defines the time interval for the hint
    /// </summary>
    /// <remarks>
    ///   Default: 1000ms
    /// </remarks>
    {$ENDREGION}
    property HintInterval: Cardinal read fHintInterval write SetHintInterval default 1000;
    {$REGION 'Defines the type of the hint'}
    /// <summary>
    ///   Defines the type of the hint
    /// </summary>
    /// <value>
    ///   <list type="table">
    ///     <listheader>
    ///       <term>Value</term>
    ///       <description>Description</description>
    ///     </listheader>
    ///     <item>
    ///       <term>Off</term>
    ///       <description>No hint (default0</description>
    ///     </item>
    ///     <item>
    ///       <term>Text</term>
    ///       <description>Shows the value of <see cref="neTabControl|TneTabControl.HintText">
    ///         HintText</see> property. If it is empty, it shows the
    ///         title of the tab <br /></description>
    ///     </item>
    ///     <item>
    ///       <term>Preview</term>
    ///       <description>Shows a preview of the tab item</description>
    ///     </item>
    ///     <item>
    ///       <term>Custom</term>
    ///       <description />
    ///     </item>
    ///   </list>
    /// </value>
    {$ENDREGION}
    property HintType: TneHintType read fHintType write fHintType default TneHintType.Off;
    {$REGION 'Enables Tab Animation'}
    /// <summary>
    ///   Enables Tab Animation
    /// </summary>
    /// <remarks>
    ///   <list type="table">
    ///     <listheader>
    ///       <term>Value</term>
    ///       <description>Description</description>
    ///     </listheader>
    ///     <item>
    ///       <term>taNone</term>
    ///       <description>No animation (Default)</description>
    ///     </item>
    ///     <item>
    ///       <term>taFade</term>
    ///       <description>Fade in</description>
    ///     </item>
    ///   </list>
    /// </remarks>
    {$ENDREGION}
    property TabAnimation: TShowTabanimation read GetTabAnimation write setTabAnimation;
    property TabAnimationDuration: double read GetTabAnimationDuration write setTabAnimationDuration;
    property TabAnimationType: TAnimationType read GetTabAnimationType write setTabAnimationType;
    property TabAnimationInterpolation: TInterpolationType read GetTabAnimationInterpolation
                write SetTabAnimationInterpolation;

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

    property OnBeforeCloseAllItems: TOnBeforeCloseAllItems
      read fOnBeforeCloseAllItems write fOnBeforeCloseAllItems;
    property OnAfterCloseAllItems: TOnAfterCloseAllItems
      read fOnAfterCloseAllItems write fOnAfterCloseAllItems;
    property OnBeforeCloseOtherItems: TOnBeforeCloseOtherItems
      read fOnBeforeCloseOtherItems write fOnBeforeCloseOtherItems;
    property OnAfterCloseOtherItems: TOnAfterCloseOtherItems
      read fOnAfterCloseOtherItems write fOnAfterCloseOtherItems;

    property OnBeforeAddSidebarControl: TOnBeforeAddSibarControl
          read fOnBeforeAddSidebarControl write fOnBeforeAddSidebarControl;
    property OnAfterAddSidebarControl: TOnAfterAddSidebarControl
          read fOnAfterAddSidebarControl write fOnAfterAddSidebarControl;
    property OnBeforeDeleteSidebarControl: TOnBeforeDeleteSidebarControl
          read fOnBeforeDeleteSidebarControl write fOnBeforeDeleteSidebarControl;
    property OnAfterDeleteSidebarControl: TOnAfterDeleteSidebarControl
          read fOnAfterDeleteSidebarControl write fOnAfterDeleteSidebarControl;

    property OnCustomHint: TOnCustomHint read fOnCustomHint write fOnCustomHint;

    //Foundation properties
    property Version: string read GetVersion;
  end;

procedure Register;

function GetTabControl (const aClass: TClass): TneTabControl;

implementation

uses
  System.Math, FMX.Dialogs, System.Generics.Defaults, System.SysUtils,
  System.StrUtils, System.UIConsts, FMX.StdCtrls, FMX.Effects, FMX.Ani, FMX.Objects;

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
    raise Exception.Create('The Tag is either empty or already exists');
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
    raise Exception.Create('The Tag is either empty, already exists or the new tab item is not assigned');
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
    raise Exception.Create('The Tag is either empty, already exists, '+
              'the new tab item is not assigned or the frame is not assigned');

  continueAdding:=True;
  if Assigned(fOnBeforeAddItem) then
    fOnBeforeAddItem(newItem, continueAdding);

  if not continueAdding then
    Exit;

  newItem.TabTag:=trimTag;
  newItem.CloseImageNormal:=fCloseImageNormal;
  newItem.CloseImageHover:=fCloseImageHover;
  newItem.CloseImagePressed:=fCloseImagePressed;
  newItem.Height:=fTabHeight;
  newItem.HintInterval:=fHintInterval;

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
  mainTab.TagString:=trimTag;
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
  fTabOrientation:=TneTabOrientation.orTop;
  fTabPosition:=TTabPosition.PlatformDefault;
  fTransitionType:=TTabTransition.None;

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

  //TneTabOrientation is Top by default
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
  fTabBar.OnChange:=OnTabBarChange;
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
  fMainControl.OnChange:=OnMainBarChange;
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

  fTabAnimation:=TShowTabanimation.taNone;
  fTabAnimationDuration:=0.2;
  fTabAnimationType:=TAnimationType.In;
  fTabAnimationInterpolation:=TInterpolationType.Linear;


  fTabsDictionary:=TDictionary<string, TneTabItem>.Create;
  fFramesDictionary:=TDictionary<string, TFrame>.Create;
  fMainTabsDictionary:=TDictionary<string, TTabItem>.Create;

  fSubscriber:=SubscriberClass;
  fSubscriber.SetUpdateSubscriberMethod(UpdateFromProvider);

  fHistoryList:=THistoryClass.Create;
  fLeftSidebarControls:=TList<TControl>.Create;
  fRightSidebarControls:=TList<TControl>.Create;

  fPopup:=TPopup.Create(self);
  fPopup.Placement:=TPlacement.Mouse;
  fPopup.HorizontalOffset:=200;
  fPopup.VerticalOffset:=200;
  fHintType:=Off;
  fCloseTimer:=TTimer.Create(self);
  fCloseTimer.Interval:=2000;
  fCloseTimer.OnTimer:=OnCloseHintTimer;
  fCloseTimer.Enabled:=false;

  fVersion:=MajorVersion+'.'+MinorVersion+'.'+BugVersion
end;

procedure TneTabControl.DeleteAllTabs(const forceDelete: Boolean);
begin
  while self.tabcount>0 do
    Self.DeleteTab(self.GetTag(0), forceDelete);
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
  tItem: TTabItem;
  delFrame: TFrame;
  i: Integer;
  lastTag: string;
  removeIndex: Integer;
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

  removeIndex:=delItem.Index;
  delItem.Visible:=false;
  fTabsDictionary.Remove(Trim(Tag));

  delFrame:=fFramesDictionary.Items[Trim(Tag)];
  delFrame.Parent:=nil;

  for i:=0 to fMainControl.TabCount-1 do
  begin
    if fMainControl.Tabs[i].TagString=Tag then
    begin
      tItem:=fMainControl.Tabs[i];
      Break;
    end;
  end;

  if Assigned(tItem) then
      tItem.Free;
  fMainTabsDictionary.Remove(Trim(Tag));

  if Assigned(fOnAfterDeleteItem) then
    fOnAfterDeleteItem(delItem);
  delItem.Free;

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
  fPopup.Free;
  fCloseTimer.Free;
  inherited;
end;

procedure TneTabControl.DoMainChange(const source: TneDoChangeType);
var
  tmpActiveTag: string;
  tmpFadeAnimation: TFloatAnimation;
  tmpFrame: TFrame;
begin

  if not Assigned(fTabBar.ActiveTab) then
    Exit;

  if fNumofDoMainChangeCalls>0 then
  begin
    fNumofDoMainChangeCalls:=0;
    Exit;
  end
  else
    Inc(fNumofDoMainChangeCalls);

  case source of
    dcTabBar: tmpActiveTag:=(fTabBar.ActiveTab as TneTabItem).TabTag;
    dcMainTab: tmpActiveTag:=fMainControl.ActiveTab.TagString;
  end;

  if (not fMainTabsDictionary.ContainsKey(tmpActiveTag)) or
       (not fFramesDictionary.ContainsKey(tmpActiveTag)) or
          (not fTabsDictionary.ContainsKey(tmpActiveTag)) then
    Exit;

  case source of
    dcTabBar: if fTransitionType=TTabTransition.Slide then
         fMainControl.SetActiveTabWithTransition(fMainTabsDictionary.Items[tmpActiveTag],
                      fTransitionType)
              else
                fMainControl.ActiveTab:=fMainTabsDictionary.Items[tmpActiveTag];
    dcMainTab: fTabBar.ActiveTab:=fTabsDictionary.Items[tmpActiveTag];
  end;

  fActiveTab:=GetTab(tmpActiveTag);
  case fTabAnimation of
    taNone: ;
    taFade: begin
              if Assigned(fActiveTab) then
              begin
                tmpFadeAnimation:=TFloatAnimation.Create(fActiveTab);
                tmpFadeAnimation.PropertyName:='Opacity';
                tmpFadeAnimation.AnimationType:=fTabAnimationType;
                tmpFadeAnimation.AutoReverse:=false;
                tmpFadeAnimation.Duration:=fTabAnimationDuration;
                tmpFadeAnimation.Trigger:='IsVisible=true';
                tmpFadeAnimation.Interpolation:=fTabAnimationInterpolation;
                tmpFadeAnimation.StartValue:=0;
                tmpFadeAnimation.StopValue:=1;
                tmpFadeAnimation.Enabled:=true;

                tmpFrame:=GetFrame(tmpActiveTag);
                if Assigned(tmpFrame) then
                begin
                  tmpFadeAnimation.Parent:=GetFrame(tmpActiveTag);
                  tmpFrame.Visible:=false;
                  tmpFrame.Visible:=true;
                end
                else
                begin
                  tmpFadeAnimation.Parent:=fActiveTab;
                  fActiveTab.Visible:=false;
                  fActiveTab.Visible:=True;
                end;
              end;
            end;
  end;

  fActiveTag:=tmpActiveTag;

  fHistoryList.AddHistory(tmpActiveTag);
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function TneTabControl.GetControl(const tag: string): TControl;
begin
  if fTabsDictionary.ContainsKey(Trim(tag)) then
    result:=fTabsDictionary.Items[trim(tag)].ControlToShow
  else
    result:=nil;
end;

function TneTabControl.GetFrame(const tag: string): TFrame;
begin
  if fFramesDictionary.ContainsKey(Trim(tag)) then
    result:=fFramesDictionary.Items[Trim(tag)]
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

function TneTabControl.GetTabAnimation: TShowTabanimation;
begin
  result:=fTabAnimation;
end;

function TneTabControl.GetTabAnimationDuration: double;
begin
  result:=fTabAnimationDuration;
end;

function TneTabControl.GetTabAnimationInterpolation: TInterpolationType;
begin
  result:=fTabAnimationInterpolation;
end;

function TneTabControl.GetTabAnimationType: TAnimationType;
begin
  result:=fTabAnimationType;
end;

function TneTabControl.GetTabByIndex(const tabIndex: Integer): TneTabItem;
var
  tmpList: TList<TneTabItem>;
begin
  if (tabIndex<-1) or (tabIndex>fTabsDictionary.Count-1) then
  begin
    result:=nil;
    exit;
  end;
  tmpList:=Tlist<TneTabItem>.Create;
  GetTabItemsList(tmpList);
  result:=tmpList.Items[tabIndex];
  tmpList.Free;
end;


function TneTabControl.GetTabCount: Integer;
begin
  result:=fTabsDictionary.Count;
end;

function TneTabControl.GetTag(const tabIndex: Integer): string;
begin
  result:=Tabs[tabIndex].TabTag;
end;

procedure TneTabControl.GetTagList(var tagList: TStringList);
var
  i: integer;
begin
  if not Assigned(tagList) then
    Exit;
  for i := 0 to fTabsDictionary.Count-1 do
    tagList.Add(Tabs[i].TabTag);
end;

function TneTabControl.GetVersion: string;
begin
  result:=fVersion;
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
  if ((Sender as TTabControl).ActiveTab is TneTabItem)
    and Assigned((Sender as TTabControl).ActiveTab) then
  begin
    tmpActiveTag:=((Sender as TTabControl).ActiveTab as TneTabItem).TabTag;
    if fMainTabsDictionary.ContainsKey(tmpActiveTag) and
         fFramesDictionary.ContainsKey(tmpActiveTag) then
    begin
      fActiveTab:=GetTab(tmpActiveTag);
      fMainControl.ActiveTab:=fMainTabsDictionary.Items[tmpActiveTag];
      fHistoryList.AddHistory(tmpActiveTag);
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
  end;
end;

procedure TneTabControl.OnCloseAllTabs(Sender: TObject);
var
  continueClose: boolean;
begin
  continueClose:=True;
  if Assigned(fOnBeforeCloseAllItems) then
    fOnBeforeCloseAllItems(continueClose);
  if continueClose then  
    DeleteAllTabs;
  if Assigned(fOnAfterCloseAllItems) then
    fOnAfterCloseAllItems;
end;

procedure TneTabControl.OnCloseHintTimer(Sender: TObject);
var
  tmpAni: TFloatAnimation;
begin
  if not fPopup.IsOpen then
    Exit;
  fCloseTimer.Enabled:=false;
  tmpAni:=TFloatAnimation.Create(fPopup);
  tmpAni.Parent:=fPopup;
  tmpAni.PropertyName:='Opacity';
  tmpAni.StartValue:=1;
  tmpAni.StopValue:=0;
  tmpAni.Duration:=0.5;
  tmpAni.Loop:=false;
  tmpAni.AutoReverse:=false;
  tmpAni.Start;
  fPopup.IsOpen:=false;
end;

procedure TneTabControl.OnCloseOtherTabs(Sender: TObject);
var
  i: Integer;
  continueClose: boolean;
begin
  continueClose:=true;
  if Assigned(fOnBeforeCloseOtherItems) then
    fOnBeforeCloseOtherItems(continueClose);
  if not continueClose then
    Exit;
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
  if Assigned(fOnAfterCloseOtherItems) then
    fOnAfterCloseOtherItems;
end;

procedure TneTabControl.OnInternalTimer(Sender: TObject);
begin
  fInternalTimer.Enabled:=false;
  case fInternalTimer.Action of
    intactDeleteTab: DeleteTab(fInternalTimer.Value);
  end;
end;

procedure TneTabControl.OnMainBarChange(Sender: TObject);
begin
  DoMainChange(TneDoChangeType.dcMainTab);
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

procedure TneTabControl.OnTabBarChange(Sender: TObject);
begin
  DoMainChange(TneDoChangeType.dcTabBar);
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

/////////////
///  There is no need to change the active tab of the MainTab control
///  because the OnChange effect of the fTabBar takes care of it
////////////
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

procedure TneTabControl.SetHintInterval(const Value: Cardinal);
begin
  fHintInterval := Value;
end;

procedure TneTabControl.setTabAnimation(const Value: TShowTabanimation);
begin
  fTabAnimation:=Value;
end;

procedure TneTabControl.setTabAnimationDuration(const Value: double);
begin
  fTabAnimationDuration:=Value;
end;

procedure TneTabControl.SetTabAnimationInterpolation(
  const Value: TInterpolationType);
begin
  fTabAnimationInterpolation:=Value;
end;

procedure TneTabControl.setTabAnimationType(const Value: TAnimationType);
begin
  fTabAnimationType:=Value;
end;

procedure TneTabControl.SetTabHeight(const newHeight: Integer);
begin
  if newHeight=0 then
    fTabHeight:=30
  else
    fTabHeight:=newHeight;
  fMainGridLayout.RowCollection.Items[0].Value:=fTabHeight;
  fMainGridLayout.ColumnCollection.Items[0].Value:=fTabHeight;
  fMainLayout.Height:=fTabHeight;
  fTabBar.TabHeight:=fTabHeight;
end;

procedure TneTabControl.SetTabOrientation(const newPos: TneTabOrientation);
begin
  fTabOrientation:=newPos;
  UpdatePosition;
end;

procedure TneTabControl.SetTabPosition(const newTabFormat: TTabPosition);
begin
  case newTabFormat of
    TTabPosition.PlatformDefault,
    TTabPosition.Top:  begin
                        fTabBar.TabPosition:=TTabPosition.Top;
                        if fTabOrientation=TneTabOrientation.orTop then
                          fMainGridLayout.RowCollection.Items[0].Value:=fTabHeight
                        else
                        if fTabOrientation=TneTabOrientation.orLeft then
                          fMainGridLayout.ColumnCollection.Items[0].Value:=fTabHeight;
                        fMainControl.TabPosition:=TTabPosition.None;
                        end;
    TTabPosition.Dots: begin
                         fTabBar.TabPosition:=TTabPosition.None;
                         if fTabOrientation=TneTabOrientation.orTop then
                           fMainGridLayout.RowCollection.Items[0].Value:=0
                         else
                         if fTabOrientation=TneTabOrientation.orLeft then
                           fMainGridLayout.ColumnCollection.Items[0].Value:=0;
                         fMainControl.TabPosition:=TTabPosition.Dots;
                       end;
    TTabPosition.None: begin
                        fTabBar.TabPosition:=TTabPosition.None;
                        if fTabOrientation=TneTabOrientation.orTop then
                          fMainGridLayout.RowCollection.Items[0].Value:=0
                        else
                        if fTabOrientation=TneTabOrientation.orLeft then
                          fMainGridLayout.ColumnCollection.Items[0].Value:=0;
                        fMainControl.TabPosition:=TTabPosition.None;
                       end;
  end;
  fTabPosition:=newTabFormat;
end;

procedure TneTabControl.ShowHint(const hTag: string; const Point:TPointF);
var
  tmpTabItem: TneTabItem;
  tmpLabel: TLabel;
  tmpRoundRect: TRoundRect;
  tmpRect: TRectangle;
  tmpBitmap: TBitmap;
  textWidth,
  textHeight: single;
  tmpHint: string;
  tmpShadow: TShadowEffect;
  tmpAni: TFloatAnimation;
  tmpControl: TControl;
  tmpImage: TImage;
  canContinue: Boolean;
begin
  for tmpControl in fPopup.Controls do
    tmpControl.Free;
  if Trim(hTag)=ActiveTag then
    Exit;

  tmpTabItem:=GetTab(Trim(hTag));
  if not Assigned(tmpTabItem) then
    Exit;

  if Trim(tmpTabItem.HintText)='' then
    tmpHint:=tmpTabItem.Text
  else
    tmpHint:=tmpTabItem.HintText;

  tmpHint:=Trim(tmpHint);
  tmpBitmap:=TBitmap.Create;
  try
    textWidth := tmpBitmap.Canvas.TextWidth(tmpHint);
    textHeight:= tmpBitmap.Canvas.TextHeight(tmpHint);
  finally
    tmpBitmap.Free;
  end;

  fPopup.Width:=textWidth+40;
  fPopup.Height:=textHeight+10;

  canContinue:=true;

  case fHintType of
    Off: ;
    Text: begin
            tmpRoundRect:=TRoundRect.Create(fPopup);
            tmpRoundRect.Parent:=fPopup;
            tmpRoundRect.Align:=TAlignLayout.Client;
            tmpRoundRect.Fill.Color:=claCornsilk;
            tmpRoundRect.Stroke.Color:=claGray;
            tmpLabel:=TLabel.Create(tmpRoundRect);
            tmpLabel.Align:=TAlignLayout.Client;
            tmpLabel.Margins.Left:=10;
            tmpLabel.Margins.Right:=10;
            tmpLabel.Margins.top:=5;
            tmpLabel.Margins.Bottom:=5;
            tmpLabel.Text:=tmpHint;
            tmpLabel.Parent:=tmpRoundRect;
          end;
    Preview: begin
               if Assigned(GetFrame(trim(hTag))) then
               begin
                 fPopup.Width:=150;
                 fPopup.Height:=100;
                 tmpRect:=TRectangle.Create(fPopup);
                 tmpRect.Parent:=fPopup;
                 tmpRect.Align:=TAlignLayout.Client;
                 tmpRect.Fill.Color:=claCornsilk;
                 tmpRect.Stroke.Color:=claGray;

                 tmpImage:=TImage.Create(tmpRect);
                 tmpImage.Parent:=fPopup;
                 tmpImage.Align:=TAlignLayout.Client;
                 tmpImage.Margins.Top:=5;
                 tmpImage.Margins.Bottom:=5;
                 tmpImage.Margins.Left:=10;
                 tmpImage.Margins.Right:=10;
                 GetTab(Trim(hTag)).PreviewImage:=GetFrame(Trim(hTag)).MakeScreenshot;
                 tmpImage.Bitmap:=GetTab(Trim(hTag)).PreviewImage;
               end;
             end;
    Custom: begin
              if Assigned(fOnCustomHint) then
              begin
                tmpControl:=TControl.Create(fPopup);
                if Assigned(fOnCustomHint) then
                begin
                  fOnCustomHint(tmpTabItem, tmpControl, canContinue);
                  if not canContinue then
                  begin
                    tmpControl.Free;
                    Exit;
                  end;
                  tmpControl.Parent:=fPopup;
                  fPopup.Width:=tmpControl.Width;
                  fPopup.Height:=tmpControl.Height;
                end
                else
                  tmpControl.Free;
              end;
            end;
  end;

  if not canContinue then
    Exit;
  tmpShadow:=TShadowEffect.Create(fPopup);
  tmpShadow.Parent:=fPopup;
  tmpShadow.Direction:=60;
  tmpShadow.Opacity:=0.6;
  tmpShadow.Softness:=0.3;

  tmpAni:=TFloatAnimation.Create(fPopup);
  tmpAni.Parent:=fPopup;
  tmpAni.PropertyName:='Opacity';
  tmpAni.StartValue:=0;
  tmpAni.StopValue:=1;
  tmpAni.Duration:=0.5;
  tmpAni.Loop:=false;
  tmpAni.AutoReverse:=false;
  tmpAni.Trigger:='IsVisible';
  fPopup.IsOpen:=True;
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

  if (receivedNotClass.Sender is TneHintTimer) then
  begin
    if fHintType=Off then
      Exit;
    if intactShowHint in receivedNotClass.Action then
      begin
        ShowHint(receivedNotClass.Value, receivedNotClass.Point);
        fCloseTimer.Enabled:=True;
      end;
    if intactHideHint in receivedNotClass.Action then
    begin
      fPopup.IsOpen:=false;
    end;
    Exit;
  end;

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
  case fTabOrientation of
    TneTabOrientation.orTop: begin
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
    TneTabOrientation.orLeft: begin
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
/////////////////////
///  The next line should not be necessary but there is a visual glitch
///  when the fTabBar abd fMainLayout rotates.
///  This is a fix
/////////////////////
                      SetTabHeight(fTabHeight);
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
