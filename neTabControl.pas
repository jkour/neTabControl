{***************************************************************}
{ This source is written by John Kouraklis.			}
{ © 2016, John Kouraklis           				}
{ Email : j_kour@hotmail.com                           		}
{                                                      		}
{ The MIT License (MIT)                                         }
{                                                               }
{ Copyright (c) 2016 John Kouraklis                             }
{                                                               }
{ Permission is hereby granted, free of charge, to any person   }
{ obtaining a copy of this software and associated documentation}
{ files (the "Software"), to deal in the Software without       }
{ restriction, including without limitation the rights to use,  }
{ copy, modify, merge, publish, distribute, sublicense, and/or  }
{ sell copies of the Software, and to permit persons to whom the}
{ Software is furnished to do so, subject to the following      }
{ conditions:                                                   }
{                                                               }
{ The above copyright notice and this permission notice shall be}
{ included in all copies or substantial portions of the Software}
{                                                               }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY     }
{ KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE    }
{ WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR       }
{ PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR }
{ COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER   }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR          }
{ OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE     }
{ SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.        }
{								}
{ Unit Name: neTabControl					}
{                                                               }
{                                                               }
{                                                               }
{***************************************************************}
unit neTabControl;

interface

uses

  System.SysUtils, System.Generics.Collections,
  FMX.Controls, FMX.TabControl, FMX.Types,
  FMX.Graphics, FMX.Forms, FMX.Menus,
  FMX.Styles.Objects,
  System.Classes,
  System.UITypes, FMX.Objects, FMX.StdCtrls, System.Rtti;


const
  MajorVersion = '0';
  MinorVersion = '3';
  BugVersion = '0';

{***************************************************************}
{ Version History:                                              }
{                                                               }
{ 0.2.0 - 05/03/2016                                            }
{                                                               }
{** Changes                                                     }
{    * Change license to MIT                                    }
{    * ShowHint disabled                                        }
{    * Check for duplicate Tag name added                       }
{    * Adjust Width improvement                                 }
{                                                               }
{** Bug                                                         }
{    * Access Violation on MouseOverTabItem when the control is }
{      host in other control (eg. a Frame)                      }
{    * AdjustWidth returns 0 when the control is within two     }
{      frames                                                   }
{                                                               }
{** New Feature                                                 }
{    * Option to disable right-click menu added                 }
{    * MinWidth property added                                  }
{    * Normal TTabItem can be added                             }
{    * Add "Close all the rest" option in popup menu            }
{                                                               }
{                                                               }
{ 0.1.0 - Initial Version (20/02/2016)                          }
{***************************************************************}

type
  THintStyle = (ShowTitle, ShowPreview, None);

  {$REGION 'Used to define which properties will be exported when the tabs are  Saved.'}
  /// <summary>
  ///   Used to define which properties will be exported when the tabs are
  ///   Saved.
  /// </summary>
  {$ENDREGION}
  TExportElements = (ExportTagInt, ExportIdentifier, ExportTagVariant);
  TExportElementsSet = set of TExportElements;


  {$REGION 'This panel is used to present the hint. It is predominantly used to display the correct style.'}
  /// <summary>
  ///   This panel is used to present the hint. It is predominantly used to
  ///   display the correct style.
  /// </summary>
  {$ENDREGION}
  THintPanel = class(TPanel)
  private
    fHintStyle: THintStyle;
    fHintText: string;
    procedure SetHintStyle (const newHintStyle: THintStyle);
    procedure SetHintText (const newHintText: string);
  protected
    procedure ApplyStyle; override;
  public
    property HintStyle: THintStyle read fHintStyle write SetHintStyle;
    property HintText: string read fHintText write SetHintText;
  end;

  TneTabItem = class(TTabItem)
  private
    fCanClose: Boolean;
    fTabControlParent: TTabControl;
    fCloseImageNormal,
    fCloseImageHover,
    fCloseImagePressed: TBitmap;
    fIcon: TBitmap;
    fShowIcon: boolean;
    fIdentifier: string;
    fTagVariant: Variant;

    fDeleted: Boolean;
    fIsMousePressed: Boolean;

    fVersion: string;
    procedure SetCloseImageNormal(const newImage: TBitmap);
    procedure SetCloseImageHover(const newImage: TBitmap);
    procedure SetCloseImagePressed (const newImage: TBitMap);

    procedure SetCanClose(const can: Boolean);
    procedure SetIcon(const newImage: TBitmap);
    procedure SetShowIcon(const can: Boolean);
    procedure AdjustTabWidth(const minW: single; const maxW: Single);

    function GetVersion: string;

    //Events
    procedure NewOnMouseEnter(ASender: TObject);
    procedure NewOnMouseLeave(ASender: TObject);
    procedure NewOnMouseMove(Sender: TObject; Shift: TShiftState; X,
                                                              Y: Single);
    procedure OnCloseImageClick(ASender: TObject);
    procedure OnCloseImageHoverClick(ASender: TObject);
    procedure OnCloseImageLeave(ASender: TObject);
    procedure OnCloseImageMouseDown(ASender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnCloseImageMouseUp(ASender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  protected
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ApplyStyle; override;
    property Deleted: boolean read fDeleted write fDeleted;
    property TabControlParent: TTabControl read fTabControlParent;
  published
    {$REGION 'Defines whether the tab can close.'}
    /// <summary>
    ///   Defines whether the tab can close.
    /// </summary>
    {$ENDREGION}
    property CanClose: boolean read fCanClose write SetCanClose;
    {$REGION 'Sets the image for the close button (normal).'}
    /// <summary>
    ///   Sets the image for the close button (normal).
    /// </summary>
    {$ENDREGION}
    property CloseImageNormal: TBitmap read fCloseImageNormal write SetCloseImageNormal;
    {$REGION 'Sets the image for the close button (normal).'}
    /// <summary>
    ///   Sets the image for the close button (normal).
    /// </summary>
    {$ENDREGION}
    property CloseImageHover: TBitmap read fCloseImageHover write SetCloseImageHover;
    {$REGION 'Sets the image for the close button (normal).'}
    /// <summary>
    ///   Sets the image for the close button (normal).
    /// </summary>
    {$ENDREGION}
    property CloseImagePressed: TBitmap read fCloseImagePressed write SetCloseImagePressed;
    {$REGION 'Sets the image for the icon which appears at the left side of the  tab.'}
    /// <summary>
    ///   Sets the image for the icon which appears at the left side of the
    ///   tab.
    /// </summary>
    {$ENDREGION}
    property Icon:TBitmap read fIcon write SetIcon;
    {$REGION 'Controls whether the icon at the left is visible.'}
    /// <summary>
    ///   Controls whether the icon at the left is visible.
    /// </summary>
    {$ENDREGION}
    property ShowIcon: boolean read fShowIcon write SetShowIcon default true;
    {$REGION 'Field used to store a TValue field. Can be used as it fits to the  developer.'}
    /// <summary>
    ///   Field used to store a sting field. Can be used as it fits to the
    ///   developer.
    /// </summary>
    {$ENDREGION}
    property Identifier: string read fIdentifier write fIdentifier;
    {$REGION 'Field to store TValue. Can be used as suits the developer.'}
    /// <summary>
    ///   Field to store TValue. Can be used as suits the developer.
    /// </summary>
    {$ENDREGION}
    property TagVariant: Variant read fTagVariant write fTagVariant;

    property Version: string read GetVersion;

    constructor Create(AOwner: TComponent); override;
  end;

  {$REGION 'This event is called before the deletion of a tab. If it is false, the  deletion is aborted. By default it returns true.'}
  /// <summary>
  ///   This event is called before the deletion of a tab. If it is false, the
  ///   deletion is aborted. By default it returns true.
  /// </summary>
  {$ENDREGION}
  TOnBeforeDelete = function (const AItem: TneTabItem): boolean of object;
  {$REGION 'This event is raised after the deletion of a tab.'}
  /// <summary>
  ///   This event is raised after the deletion of a tab.
  /// </summary>
  /// <remarks>
  ///   ADeletedItem MUST BE freed in the event handler.
  /// </remarks>
  {$ENDREGION}
  TOnAfterDelete = procedure (ADeletedItem: TneTabItem; ADeletedFrame: TFrame) of object;

  TSetOfString = array of string;

  TneTabControl = class(TTabControl)
  private
    fDictionaryTabs: TDictionary<string, TneTabItem>;
    fDictionaryFrames: TDictionary<string, TFrame>;
    fHintStyle: THintStyle;
    fHintShowDelay: integer;
    fHintTimer: TTimer;
    fHintPopup: TPopup;
    fHintPanel: THintPanel;
    fCloseImageNormal,
    fCloseImageHover,
    fCloseImagePressed: TBitmap;
    fVersion: string;
    fCurrentNETabItem: TneTabItem;

    //Events
    fOnBeforeDelete: TOnBeforeDelete;
    fOnAfterDelete: TOnAfterDelete;

    //Published properties
    fPopupBeforeDefaultMenu: TPopupMenu;
    fPopupAfterDefaultMenu: TPopupMenu;
    fCloseTabLabel: string;
    fMaxTabWidth: single;
    fMinTabWidth: single;
    fDisablePopupMenu: boolean;
    fCloseAllOtherTabsLabel: string;
    fCloseTabOnDoubleClick: boolean;

    //Holds the history of the clicked tags
    fHistoryTags: TList<string>;
    function GetLastVisitedTag: string;

    //Events
    procedure OnHintTimer(Sender:TObject);

    //Popup Events
    procedure OnPopUpMenuClose(ASender: TObject);
    procedure OnPopUpChangeTab(ASender: TObject);
    procedure OnPopUpMenuCloseAllOtherTabs (ASender: TObject);

    //Procedures-Functions
    procedure ShowPopUpMenu(const ASender:TNETabItem; const X, Y: Single);
    procedure SetHintSTyle(const showH: THintStyle);
    procedure SetHintShowDelay(const delay: integer);
    procedure SetMaxTabWidth (const newW: Single);
    procedure SetMinTabWidth (const newW: Single);
    {$REGION 'This method shows the hint on the tabitem.'}
    /// <summary>
    ///   This method shows the hint on the tabitem.
    /// </summary>
    {$ENDREGION}
    procedure MouseEnterTabItem (ASender: TObject);
    procedure MouseMoveOverTabItem(const tag: string; const X, Y: Single);
    procedure MouseExitTabItem;

    function GetActiveTag: string;

    function GetVersion: string;
    procedure CleanUpTabs;
  protected
    procedure DoChange; override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddTab(const tag: string; var newFrame: TFrame); overload;
    procedure AddTab(const tag: string; var newItem: TneTabItem;
      var newFrame: TFrame); overload;
    procedure AddTab(const tag: string; var newItem: TTabItem;
      var newFrame: TFrame); overload;

    {$REGION 'Deletes a tab using the tag.'}
    /// <summary>
    ///   Deletes a tab using the tag.
    /// </summary>
    /// <param name="forceDelete">
    ///   By default it is False. The CanClose property defines whether a tag
    ///   can be deleted. If true value is passed, the tab is deleted
    ///   regardless the CanClose value
    /// </param>
    {$ENDREGION}
    procedure DeleteTab(const tag: string; const forceDelete: Boolean = false);
    procedure DeleteAllTabs (const forceDelete: boolean = false);

    procedure InsertTab(const tag: string; const index: Integer;
                                              var newFrame: TFrame);overload;
    procedure InsertTab(const tag: string; const index: Integer;
                      var newItem: TneTabItem; var newFrame:TFrame); overload;

    procedure SetActiveTab(const tag: string);

    {$REGION 'Saves the open tabs to a list of strings.'}
    /// <summary>
    ///   Saves the open tabs to a list of strings.
    /// </summary>
    /// <param name="exportElements">
    ///   Defines which elements will be used in the export data. See <see cref="neTabControl|TExportElementsSet" />
    /// </param>
    /// <param name="excludeTabs">
    ///   Defines which tabs (by tag) will be excluded from the save list
    /// </param>
    /// <param name="savedTabs">
    ///   The list of saved tabs
    /// </param>
    /// <remarks>
    ///   Export Structure (tabs are exported as they appear in the tab)
    ///   <list type="bullet">
    ///     <item>
    ///       [tag] = the tag of the netabitem <br /><br />
    ///     </item>
    ///     <item>
    ///       [tagIdentifier] = the Identifier property of the neTabItem if
    ///       selected in exportElements <br /><br />
    ///     </item>
    ///     <item>
    ///       [tagInt] = the tag property of the neTabitem if selected in
    ///       exportElements <br /><br />
    ///     </item>
    ///     <item>
    ///       [tagVariant] = the tagVariant property of the neTabitem if
    ///       selected in exportElements <br />
    ///     </item>
    ///   </list>
    /// </remarks>
    {$ENDREGION}
    procedure SaveTabs(const exportElements: TExportElementsSet;
     const excludeTabs: TSetOfString; var savedTabs: TStringList);
    function GetTab(const tag: string): TneTabItem;
    function GetFrame(const tag: string): TFrame;
    function GetTabsTags: TStringList;
    function GetTabsText: TStringList;
    function FindTabOrderFromTag(const Tag: string): Integer;
    function FindTagFromTabOrder(const index: integer): string;

    property ActiveTag: string read GetActiveTag;
  published
    //Events
    property OnBeforeDelete: TOnBeforeDelete read fOnBeforeDelete write fOnBeforeDelete;
    property OnAfterDelete: TOnAfterDelete read fOnAfterDelete write fOnAfterDelete;
    {$REGION 'This method is called before the construction of the default menu  items. Use this method to add menu items in the beginning of the  popup menu.'}
    /// <summary>
    ///   This method is called before the construction of the default menu
    ///   items. Use this method to add menu items in the beginning of the
    ///   popup menu.
    /// </summary>
    {$ENDREGION}
    property PopupBeforeDefault:TPopupMenu read fPopupBeforeDefaultMenu
      write fPopupBeforeDefaultMenu;
    {$REGION 'This method is called after the construction of the default menu  items. Use this method to add menu items at the end of the popup  menu.'}
    /// <summary>
    ///   This method is called after the construction of the default menu
    ///   items. Use this method to add menu items at the end of the popup
    ///   menu.
    /// </summary>
    {$ENDREGION}
    property PopupAfterDefault: TPopupMenu read fPopupAfterDefaultMenu
      write fPopupAfterDefaultMenu;

    //Properties
    {$REGION 'If true shows the text of the tab as hint.'}
    /// <summary>
    ///   If true shows the text of the tab as hint.
    /// </summary>
    {$ENDREGION}
    property HintStyle: THintStyle read fHintStyle write SetHintSTyle;
    {$REGION 'Defines the label for the Close menu in the popup menu.'}
    /// <summary>
    ///   Defines the label for the Close menu in the popup menu.
    /// </summary>
    {$ENDREGION}
    property CloseTabLabel: string read fCloseTabLabel write fCloseTabLabel;
    {$REGION 'Defines the minimum tab width.'}
    /// <summary>
    ///   Defines the minimum tab width.
    /// </summary>
    {$ENDREGION}
    property MinTabWidth: Single read fMinTabWidth write SetMinTabWidth;
    {$REGION 'Defines the maximum tab width.'}
    /// <summary>
    ///   Defines the maximum tab width.
    /// </summary>
    /// <remarks>
    ///   Default value: 120px
    /// </remarks>
    {$ENDREGION}
    property MaxTabWidth: single read fMaxTabWidth write SetMaxTabWidth;
    {$REGION 'The image for the close pic (normal state).'}
    /// <summary>
    ///   The image for the close pic (normal state).
    /// </summary>
    {$ENDREGION}
    property CloseImageNormal: TBitmap read fCloseImageNormal write fCloseImageNormal;
    property CloseImageHover: TBitmap read fCloseImageHover write fCloseImageHover;
    property CloseImagePressed: TBitmap read fCloseImagePressed write fCloseImagePressed;
    {$REGION 'Disables the popup menu when the user right-clicks on a tab.'}
    /// <summary>
    ///   Disables the popup menu when the user right-clicks on a tab.
    /// </summary>
    {$ENDREGION}
    property DisablePopupMenu: boolean read fDisablePopupMenu write fDisablePopupMenu;
    {$REGION 'The label for the "Close All Other Tabs" option in the popup menu.'}
    /// <summary>
    ///   The label for the "Close All Other Tabs" option in the popup menu.
    /// </summary>
    {$ENDREGION}
    property CloseAllOtherTabsLabel: string read fCloseAllOtherTabsLabel
             write fCloseAllOtherTabsLabel;
    {$REGION 'Sets the delay before the hint is shown.'}
    /// <summary>
    ///   Sets the delay before the hint is shown.
    /// </summary>
    /// <value>
    ///   Value in ms.
    /// </value>
    /// <remarks>
    ///   Default Value: 500ms
    /// </remarks>
    {$ENDREGION}
    property HintShowDelay: integer read fHintShowDelay write SetHintShowDelay default 500;
    property CloseTabOnDoubleClick: boolean read fCloseTabOnDoubleClick
             write fCloseTabOnDoubleClick default false;

    property Version: string read GetVersion;
  end;

procedure Register;


{$REGION 'Global function to be used by frames. Returns the parent neTabItem.'}
/// <summary>
///   Global function to be used by frames. Returns the parent neTabItem.
/// </summary>
{$ENDREGION}
function FindParentNeTabItem(const checkComponent: TFMXObject):TneTabItem;

implementation

uses
  System.Math, FMX.Dialogs, System.Types, System.Generics.Defaults;

{General Utils}

function ResizeBitmap(B: TBitmap; width, height: Integer): TBitmap;
begin
  result:=TBitmap.Create(width, height);
  Result.Clear(0);
  if result.Canvas.BeginScene then
  try
    Result.Canvas.DrawBitmap(B, RectF(0,0,b.Width, b.Height), RectF(0,0,width, height),1);
  finally
    Result.Canvas.EndScene;
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

{ Register}

procedure Register;
begin
  RegisterComponents('NusEnvision', [TneTabControl]);
end;

{ TneTabControl }

procedure TneTabControl.AddTab(const tag: string; var newFrame: TFrame);
var
  tmpItem: TNeTabItem;
  str: string;
begin
  if trim(tag)='' then
    Exit;
  tmpItem:=TneTabItem.create(self);
  str:='Tab '+self.TabCount.ToString;
  tmpItem.Text:=str;
  AddTab(tag, tmpItem, newFrame);
end;

procedure TneTabControl.AddTab(const tag: string; var newItem: TneTabItem;
  var newFrame: TFrame);
begin
  if trim(tag)='' then Exit;
  if not Assigned(newitem) then Exit;

  try
    if fDictionaryTabs.ContainsKey(tag) then Exit;
    if fDictionaryFrames.ContainsKey(tag) then Exit;

    fdictionaryTabs.Add(trim(tag), newItem);
    fDictionaryFrames.Add(trim(tag), newFrame);

    newItem.Parent:=self;
    newItem.TagString:=trim(tag);
    newItem.Deleted:=False;

    newItem.CloseImageNormal:=fCloseImageNormal;
    newItem.CloseImageHover:=fCloseImageHover;
    newItem.CloseImagePressed:=fCloseImagePressed;

    if Assigned(newFrame) then
    begin
      newFrame.Parent:=newItem;
      newFrame.TagString:=trim(tag);
      newItem.AddObject(newFrame);
    end;

    if fDictionaryTabs.Count=1 then
      fHistoryTags.Add(newItem.TagString);

    self.AddObject(newItem);

    newItem.AdjustTabWidth(fMinTabWidth, fMaxTabWidth);

    //this is to fix the empty entry in the very beggining (no tabs)
    if fHistoryTags.Count>0 then
    begin
      if trim(fHistoryTags.Items[0])='' then
        fHistoryTags.Delete(0);
    end
    else
      if (self.tabcount>0) and (self.tabindex>-1) then
        fHistoryTags.Add(self.activeTab.tagstring);
  except
    raise ;
  end;

end;

procedure TneTabControl.AddTab(const tag: string; var newItem: TTabItem;
  var newFrame: TFrame);
var
  tmpneItem: TneTabItem;

begin

  if trim(tag)='' then Exit;
  if not Assigned(newitem) then Exit;
  if not Assigned(fDictionaryTabs) then Exit;
  if not Assigned(fDictionaryFrames) then Exit;

  if fDictionaryTabs.ContainsKey(tag) then Exit;
  if fDictionaryFrames.ContainsKey(tag) then Exit;

  tmpneItem:=TneTabItem.Create(newItem.Owner);
  tmpneItem.Text:=newItem.Text;
  tmpneItem.TagString:=trim(tag);
  tmpneItem.Parent:=self;
  tmpneItem.Deleted:=false;
  tmpneItem.CanClose:=False;
  tmpneItem.ShowIcon:=False;
  tmpneItem.AutoSize:=True;

  if Assigned(newFrame) then
  begin
    newFrame.Parent:=tmpneItem;
    newFrame.TagString:=trim(tag);
    tmpneItem.AddObject(newFrame);
  end;

  fdictionaryTabs.Add(trim(tag), tmpneItem);
  fDictionaryFrames.Add(trim(tag), newFrame);

  if fDictionaryTabs.Count=1 then
    fHistoryTags.Add(newItem.TagString);

  self.AddObject(tmpneItem);

  //this is to fix the empty entry in the very beggining (no tabs)
  if (fHistoryTags.Count>0) and (fHistoryTags.Items[0]='') then
    fHistoryTags.Delete(0);

end;

constructor TneTabControl.Create(AOwner: TComponent);
begin
  inherited;
  fDictionaryTabs:=TDictionary<string, TneTabItem>.Create;
  fDictionaryFrames:=TDictionary<string, TFrame>.Create;

  fHistoryTags:=TList<string>.Create;

  fCloseTabLabel:='Close Tab';

  fDisablePopupMenu:=False;

  fPopupBeforeDefaultMenu:=nil;
  fPopupAfterDefaultMenu:=nil;

  fMaxTabWidth:=120;
  fMinTabWidth:=80;

  fCloseAllOtherTabsLabel:='Close All Other Tabs';

  self.StyleLookup:='netabcontrolstyle';

  fHintShowDelay:=500;
  fHintTimer:=TTimer.Create(self);
  fHintTimer.Interval:=fHintShowDelay;
  fHintTimer.OnTimer:=OnHintTimer;
  fHintTimer.Enabled:=false;

  fHintPopup:=TPopup.Create(self);
end;

procedure TneTabControl.DblClick;
begin
  inherited;
  if fCloseTabOnDoubleClick then
    Self.DeleteTab(GetActiveTag);
end;

procedure TneTabControl.DeleteAllTabs (const forceDelete: boolean = false);
var
  tmpList: TStringList;
  str: string;
begin
  tmpList:=self.GetTabsTags;
  for str in tmpList do
    self.DeleteTab(str, forceDelete);
  tmpList.Free;
end;

procedure TneTabControl.DeleteTab(const tag: string; const forceDelete: Boolean = false);
var
  tag2,
  lastvisitedTag: string;
  currentIndex,
  deleteindex: Integer;
  deletedItem,
  tmpItem: TneTabItem;
  deletedFrame,
  leftFrame: TFrame;

begin
  tag2:=Trim(tag);

  if self.TabCount=0 then exit;

  if not Assigned(fDictionaryTabs) then Exit;
  if not fDictionaryTabs.ContainsKey(tag2) then Exit;
  if (not fDictionaryTabs.Items[tag2].CanClose)
     and (not forceDelete) then
       Exit;

  if not Assigned(fDictionaryFrames) then Exit;
  if not fDictionaryFrames.ContainsKey(tag2) then Exit;

  if Assigned(fOnBeforeDelete) then
    if not fOnBeforeDelete(fDictionaryTabs.Items[tag2]) then
      Exit;

  self.GetTab(tag2).Visible:=false;
  fDictionaryTabs.Items[tag2].Deleted:=true;

  lastvisitedTag:=GetLastVisitedTag;
  if lastvisitedTag='' then
  begin
    if self.TabCount>0 then
      self.SetActiveTab(FindTagFromTabOrder(0));
  end
  else
    self.SetActiveTab(lastvisitedTag);

  deletedItem:=nil;
  if fDictionaryTabs.ContainsKey(tag2) then
  begin
    tmpItem:=fDictionaryTabs.Items[tag2];
    if Assigned(tmpItem) then
    begin
      deletedItem:=TneTabItem.Create(nil);
      deletedItem.Text:=tmpItem.Text;
      deletedItem.CanClose:=tmpItem.CanClose;
      deletedItem.TagString:=tmpItem.TagString;
    end;
  end;

  deletedFrame:=nil;
//  if fdictionaryFrames.ContainsKey(tag2) then
//    deletedFrame:=TFrame(fDictionaryFrames.Items[tag2].
//       Clone(fdictionaryFrames.Items[tag2]));

  if Assigned(fOnAfterDelete) then
    fOnAfterDelete(deletedItem, deletedFrame);

  CleanUpTabs;
end;

destructor TneTabControl.Destroy;
var
  tmpItem: TneTabItem;
begin
  for tmpItem in fDictionaryTabs.Values do
  begin
    tmpItem.Icon.Free;
    tmpItem.Free;
  end;
  fCloseImageNormal.Free;
  fCloseImageHover.Free;
  fCloseImagePressed.Free;
  fDictionaryTabs.Free;
  fDictionaryFrames.Free;
  fHistoryTags.Free;
  fHintTimer.Free;
  fHintPopup.Free;
  inherited;
end;

function TneTabControl.FindTabOrderFromTag(const Tag: string): Integer;
var
  I: Integer;
begin
  result:=-1;
  for I := 0 to Self.TabCount-1 do
    if (trim(Tag)=Trim(self.tabs[i].TagString))
      and (self.Tabs[i].Visible) then
    begin
      result:=i;
      exit;
    end;
end;

function TneTabControl.FindTagFromTabOrder(const index: integer): string;
var
  tmpList: TList<TneTabItem>;
  tmpItem: TneTabItem;
  tmpComparison: TComparison<TneTabItem>;
begin
  if (not index>=0) and (index<=self.TabCount-1) then exit;
  if (not Assigned(fDictionaryTabs)) or (fDictionaryTabs.Count=0) then Exit;

  tmpComparison:=
    function (const Left, Right: TneTabItem): integer
    begin
      result:=TComparer<integer>.Default.Compare(left.Index, right.Index);
    end;

  tmpList:=TList<TneTabItem>.Create;
  for tmpItem in fDictionaryTabs.Values do
    tmpList.Add(tmpItem);
  tmpList.Sort(TComparer<TneTabItem>.Construct(tmpComparison));

  result:=tmpList.Items[0].TagString;
  tmpList.Free;
end;

procedure TneTabControl.CleanUpTabs;
var
  key: string;
  i: Integer;
begin
  //Tidy up the dictionaries
  //Delete the invisible tabs
  for key in fDictionaryTabs.Keys do
    if fDictionaryTabs.Items[key].Deleted then
    begin
      fDictionaryTabs.Remove(key);
      if fdictionaryFrames.containsKey(key) then
        fDictionaryFrames.Remove(key);
      i := FindTabOrderFromTag(key);
      self.delete(i);
    end;
end;


function TneTabControl.GetActiveTag: string;
begin
  if Assigned(self.ActiveTab) then
    result:=self.ActiveTab.TagString
  else
  begin
    self.SetActiveTab(GetLastVisitedTag);
    result:=GetLastVisitedTag;
  end;
end;

function TneTabControl.GetFrame(const tag: string): TFrame;
begin
  if not Assigned(fDictionaryFrames) then
    result:=nil
  else
    if (fDictionaryTabs.ContainsKey(trim(tag)))
      and (not fDictionaryTabs.Items[trim(tag)].Deleted) then
        if fDictionaryFrames.ContainsKey(trim(tag)) then
          result:=fDictionaryFrames.Items[trim(tag)]
        else
          result:=nil
    else
      result:=nil;
end;

function TneTabControl.GetLastVisitedTag: string;
begin
    if Assigned(fHistoryTags) then
    begin
       if (fHistoryTags.Count>0) then
        begin
          fHistoryTags.Delete(fHistoryTags.Count-1);
          if fHistoryTags.Count>0 then
          begin
            if (fDictionaryTabs.ContainsKey(fHistoryTags.Items[fHistoryTags.Count-1]))
                  and (not fDictionaryTabs.Items[fHistoryTags.Items[fHistoryTags.count-1]].Deleted) then
              result:=GetLastVisitedTag
            else
              result:=fHistoryTags.Items[fHistoryTags.Count-1];
          end
          else
            result:='';
        end
      else
        result:='';
    end
    else
      Result:='';
end;

function TneTabControl.GetTab(const tag: string): TneTabItem;
begin
  if not Assigned(fDictionaryTabs) then
    result:=nil
  else
    if (fDictionaryTabs.ContainsKey(Trim(tag))) then
      if (not fDictionaryTabs.Items[trim(tag)].Deleted) then
        result:=fDictionaryTabs.Items[trim(tag)]
      else
        result:=nil
    else
      result:=nil;
end;

function TneTabControl.GetTabsTags: TStringList;
var
  tmpList: TStringList;
  i: Integer;
begin
  tmpList:=TStringList.Create;
  for i := 0 to self.TabCount-1 do
    if self.Tabs[i].Visible then
      tmpList.Add(self.Tabs[i].TagString);
  result:=tmpList;
end;

function TneTabControl.GetTabsText: TStringList;
var
  tmpList: TStringList;
  i: Integer;
begin
  tmpList:=TStringList.Create;
  for i := 0 to self.TabCount-1 do
    if self.Tabs[i].Visible then
      tmpList.Add(self.Tabs[i].Text);
  result:=tmpList;
end;

function TneTabControl.GetVersion: string;
begin
  result:=MajorVersion+'.'+MinorVersion+'.'+BugVersion;
end;

procedure TneTabControl.InsertTab(const tag: string; const index: Integer;
  var newItem: TneTabItem; var newFrame: TFrame);
begin
  if trim(tag)='' then Exit;
  if not Assigned(newitem) then Exit;
  if ((index<0) or (index>TabCount-1)) and (self.TabCount>0) then
    Exit;

//InsertObject has a bug as it doesn't insert tabs in the index-location
//self.InsertObject(index, newItem);
//We first add the tab and then rearrange it by changing the index property

  self.AddTab(tag, newItem, newFrame);
  newItem.Index:=index;
end;

procedure TneTabControl.MouseEnterTabItem(ASender: TObject);
begin
  if fHintStyle=THintStyle.None then
    Exit
  else
  begin
    if (ASender is TneTabItem) then
    begin
      fCurrentNETabItem:=ASender as TneTabItem;
      fHintTimer.Enabled:=true;
    end;
  end;
end;

procedure TneTabControl.MouseExitTabItem;
begin
  fHintTimer.Enabled:=false;
  if Assigned(fHintPopup) then
    fHintPopup.IsOpen:=false;
end;

procedure TneTabControl.MouseMoveOverTabItem(const tag: string; const X,
  Y: Single);
begin
  if fHintStyle=THintStyle.None then
    Exit;
  fHintTimer.Enabled:=False;
  fHintTimer.Enabled:=true;
end;

procedure TneTabControl.InsertTab(const tag: string; const index: Integer;
  var newFrame: TFrame);
var
  tmpItem: TNeTabItem;
  str: string;
begin
  tmpItem:=TneTabItem.create(self);
  str:='Tab '+self.TabCount.ToString;
  tmpItem.Text:=str;
  InsertTab(tag, index, tmpItem, newFrame);
end;

procedure TneTabControl.OnHintTimer(Sender: TObject);
var
  tag: string;
  tmpBitImage,
  tmpImage2: TBitmap;
  tmpImageControl: TImageControl;
  tmpFrame: TFrame;

begin
  if not Assigned(fCurrentNETabItem) then Exit;
  if fHintStyle=THintStyle.None then Exit;

  tag:=fCurrentNETabItem.TagString;
  if ActiveTag=tag then Exit;

  if not fDictionaryTabs.ContainsKey(tag) then Exit;

  fHintPopup.Placement:=TPlacement.BottomCenter;
  fHintPopup.PlacementTarget:=fCurrentNETabItem;

  fHintPanel:=THintPanel.Create(self);
  fHintPanel.Align:=TAlignLayout.Client;
  fHintPanel.HintStyle:=fHintStyle;

  case fHintStyle of
    ShowTitle: begin
                 fHintPanel.HintText:=fCurrentNETabItem.Text;
                 fHintPopup.Width:=300;
                 fHintPopup.Height:=100;
               end;
    ShowPreview: begin
                   fHintPanel.HintText:='';
                   if not fdictionaryFrames.containsKey(tag) then Exit;
                   tmpFrame:=GetFrame(tag);
                   if not Assigned(tmpFrame) then Exit;
                   tmpBitImage:=tmpFrame.MakeScreenshot;
                   fHintPopup.Width:=200;
                   fHintPopup.Height:=200;
                   if Assigned(tmpBitImage) then
                   begin
                     tmpImage2:=ResizeBitmap(tmpBitImage, round(fHintPopup.Width)
                                                  , round(fHintPopup.Height));
                     tmpImageControl:=TImageControl.Create(fHintPanel);
                     tmpImageControl.Align:=TAlignLayout.Client;
                     tmpImageControl.Bitmap.Assign(tmpimage2);
                     fHintPanel.AddObject(tmpImageControl);
                     tmpImage2.free;
                     tmpBitImage.Free;
                   end;
                 end;
  end;

  fHintPopup.HorizontalOffset:=fCurrentNETabItem.Width/3;
  fHintPopup.VerticalOffset:=-5;
  fHintPopup.AddObject(fHintPanel);

  fHintPopup.IsOpen:=true;
end;

procedure TneTabControl.OnPopUpChangeTab(ASender: TObject);
begin
  if ASender is TMenuItem then
    SetActiveTab((ASender as TMenuItem).TagString);
end;

procedure TneTabControl.OnPopUpMenuClose(ASender: TObject);
begin
  if ASender is TMenuItem then
    DeleteTab((ASender as TMenuItem).TagString);
end;

procedure TneTabControl.OnPopUpMenuCloseAllOtherTabs(ASender: TObject);
var
  key: string;
begin
  if ASender is TMenuItem then
  begin
    CleanUpTabs;
    SetActiveTab((ASender as TMenuItem).TagString);
    for key in fDictionaryTabs.Keys do
      if trim(key)<>trim((ASender as TMenuItem).TagString) then
          DeleteTab(key);
  end;
end;

procedure TneTabControl.DoChange;
begin
  if Self.HasActiveTab then
    fHistoryTags.Add(self.ActiveTab.TagString);
  CleanUpTabs;
  inherited;
end;

procedure TneTabControl.SaveTabs(const exportElements: TExportElementsSet;
     const excludeTabs: TSetOfString; var savedTabs: TStringList);
var
  tmpString,
  tmpTag, str: string;
  i: Integer;
  tmpneTabItem: TneTabItem;

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
  ///  [tagIdentifier] = the Identifier property of the neTabItem if selected in exportElements
  ///  [tagInt] = the tag property of the neTabitem if selected in exportElements
  ///  [tagVariant] = the tagVariant property of the neTabitem if selected in exportElements
  savedTabs.Clear;
  CleanUpTabs;
  for i := 0 to self.TabCount-1 do
  begin
    tmpString:='';
    tmpTag:=self.Tabs[i].TagString;
    if not IsTagInExludeTabs(tmpTag) then
    begin
      if fDictionaryTabs.ContainsKey(tmpTag) then
      begin
        tmpString:=tmpString+tmpTag;
        if ExportIdentifier in exportElements then
        begin
          tmpneTabItem:=fDictionaryTabs.Items[tmpTag];
          if Assigned(tmpneTabItem) then
            tmpString:=tmpString+','+tmpneTabItem.Identifier;
        end;

        if ExportTagInt in exportElements then
        begin
          tmpneTabItem:=fDictionaryTabs.Items[tmpTag];
          if Assigned(tmpneTabItem) then
          begin
            tmpString:=tmpString+','+tmpneTabItem.Tag.ToString;
          end;
        end;

        if ExportTagVariant in exportElements then
        begin
          tmpneTabItem:=fDictionaryTabs.Items[tmpTag];
          if Assigned(tmpneTabItem) then
          begin
            if tmpneTabItem.TagVariant<>'' then
              str:=trim(string(tmpneTabItem.TagVariant));
            if str<>'' then
              tmpString:=tmpString+','+str;
          end;
        end;
        savedTabs.Add(tmpString);
      end;
    end;
  end;
end;

procedure TneTabControl.SetActiveTab(const tag: string);
var
  tag2: string;
begin
  tag2:=trim(tag);
  if tag2='' then
    exit
  else
  begin
    if (fDictionaryTabs.ContainsKey(tag2)) and
      (fDictionaryTabs.Items[tag2].Visible) then
      self.ActiveTab:=self.Tabs[FindTabOrderFromTag(tag2)];
  end;
end;


procedure TneTabControl.SetMaxTabWidth(const newW: Single);
begin
  if newW=0 then
    fMaxTabWidth:=120
  else
    fMaxTabWidth:=newW;
end;

procedure TneTabControl.SetMinTabWidth(const newW: Single);
begin
  if newW=0 then
    fMinTabWidth:=80
  else
    fMinTabWidth:=newW;
end;

procedure TneTabControl.SetHintShowDelay(const delay: integer);
begin
  fHintShowDelay:=delay;
  fHintTimer.Interval:=fHintShowDelay;
end;

procedure TneTabControl.SetHintStyle(const showH: THintStyle);
begin
  fHintStyle:=showH;
end;

procedure TneTabControl.ShowPopUpMenu(const ASender: TNETabItem; const X,
  Y: Single);
var
  tmpPopMenu: TPopUpMenu;
  tmpMenuItem: TMenuItem;
  FP: TPointF;
  tmpForm: TForm;
  i: Integer;
  tmpItem: TNETabItem;
  AtLeastOneTabCanClose: boolean;
  key: string;
begin
  if not Assigned(ASender) then Exit;

  if fDisablePopupMenu then
     Exit;

  tmpPopMenu:=TPopupMenu.Create(Self);
  tmpPopMenu.Parent:=self;

  if Assigned(fPopupBeforeDefaultMenu) then
  begin
    for i:=0 to fPopupBeforeDefaultMenu.ItemsCount-1 do
    begin
      tmpMenuItem:=fPopupBeforeDefaultMenu.Items[i];
      tmpMenuItem:=tmpMenuItem.Clone(tmpMenuItem) as TMenuItem;
      tmpMenuItem.Parent:=tmpMenuItem.Parent;
      tmpMenuItem.TagString:=ASender.TagString;
      tmpMenuItem.OnClick:=fPopupBeforeDefaultMenu.Items[i].OnClick;
      tmpPopMenu.AddObject(tmpMenuItem);
    end;
  end;

  //Close menu item
  if ASender.CanClose then
  begin
    tmpMenuItem:=TMenuItem.Create(tmpPopMenu);
    tmpMenuItem.Text:=fCloseTabLabel;
    tmpMenuItem.TagString:=ASender.TagString;
    tmpMenuItem.OnClick:=OnPopUpMenuClose;

    tmpPopMenu.AddObject(tmpMenuItem);
  end;

  //Close All Other Tabs item
  AtLeastOneTabCanClose:=false;
  for key in fDictionaryTabs.Keys do
  begin
    tmpItem:=fDictionaryTabs.Items[key];
    if Assigned(tmpItem) then
       if tmpItem.CanClose and (tmpItem<>ASender) then
          AtLeastOneTabCanClose:=true;
       end;

  if AtLeastOneTabCanClose then
  begin
    tmpMenuItem:=TMenuItem.Create(tmpPopMenu);
    tmpMenuItem.Text:=fCloseAllOtherTabsLabel;
    tmpMenuItem.TagString:=ASender.TagString;
    tmpMenuItem.OnClick:=OnPopUpMenuCloseAllOtherTabs;
    tmpPopMenu.AddObject(tmpMenuItem);
  end;


  if tmpPopMenu.ItemsCount>0 then
  begin
    tmpMenuItem:=tMenuItem.create(tmpPopMenu);
    tmpMenuItem.Text:='-';
    tmpPopMenu.AddObject(tmpMenuItem);
  end;

  for i := 0 to Self.TabCount-1 do
  begin
    tmpitem:=self.Tabs[i] as TNETabItem;
    if assigned (tmpItem) and (tmpItem.Visible) then
    begin
      tmpMenuItem:=TMenuItem.Create(tmpPopMenu);
      tmpMenuItem.Text:=tmpItem.Text;
      tmpMenuItem.TagString:=tmpItem.TagString;
      tmpMenuItem.OnClick:=OnPopUpChangeTab;
      if Self.HasActiveTab then
        if tmpItem=(Self.ActiveTab as TneTabItem) then
          tmpMenuItem.IsChecked:=True;
      tmpPopMenu.AddObject(tmpMenuItem);
    end;
  end;

  if Assigned(fPopupAfterDefaultMenu) then
  begin
    for i:=0 to fpopupAfterDefaultMenu.ItemsCount-1 do
    begin
      tmpMenuItem:=fpopupAfterDefaultMenu.Items[i];
      tmpMenuItem:=tmpMenuItem.Clone(tmpMenuItem) as TMenuItem;
      tmpMenuItem.Parent:=fPopupAfterDefaultMenu.Items[i].Parent;
      tmpMenuItem.OnClick:=fpopupAfterDefaultMenu.Items[i].OnClick;
      tmpMenuItem.TagString:=ASender.TagString;
      tmpPopMenu.AddObject(tmpMenuItem);
    end;
  end;

  FP := TPointF.Create(X, Y);
  FP := ASender.LocalToAbsolute(FP);

  tmpForm:=FindParentForm(self);
  if Assigned(tmpForm) then
  begin
    FP:=tmpForm.ClientToScreen(FP);
    tmpPopMenu.Popup(FP.X, FP.Y);
  end;

end;

{ TneTabItem }

procedure TneTabItem.AdjustTabWidth(const minW: single; const maxW: Single);
var
  tmpItem: TneTabItem;
  tmpWidth,
  tmpHeight: single;
  tmpFMX: TFmxObject;
  tmpImage: TImage;
begin

  tmpItem:=self;
  tmpheight:=self.Height;

  tmpItem.AutoSize:=true;
  tmpWidth:=self.Width;
  tmpWidth:=Min(maxW, tmpWidth);
  tmpWidth:=Max(minW, tmpWidth);

  tmpFMX:=self.FindStyleResource('closeimage');
  if tmpFMX is TImage then
  begin
    tmpImage:=TImage(tmpFMX);
    if not Assigned(tmpImage) then Exit;
    tmpWidth:=tmpWidth+tmpimage.Width+tmpimage.Padding.Left+
                tmpImage.Padding.right;
    tmpImage.Free;
  end;

  tmpFMX:=self.FindStyleResource('iconimage');
  if (tmpFMX is TImage) then
  begin
    tmpImage:=TImage(tmpFMX);
    if Assigned(tmpImage) then
    begin
      if fShowIcon then
      begin
        tmpWidth:=tmpWidth+tmpimage.Width+tmpimage.Padding.Left+
                  tmpImage.Padding.Right;
        tmpImage.Visible:=true;
      end
      else
        tmpImage.Visible:=false;
      tmpImage.Free;
    end;
  end;

  self.AutoSize:=false;
  self.Width:=round(tmpWidth);
  Self.Height:=round(tmpHeight);

end;


procedure TneTabItem.ApplyStyle;
var
  tmpFMX: TFMXObject;
  tmpImage: TImage;
  Image2: TBitmap;
begin
  inherited;
  //close image
  tmpFMX:=FindStyleResource('closeimage');
  if (tmpFMX is TImage) then
  begin
    tmpImage:=TImage(tmpFMX);
    tmpImage.OnClick:=OnCloseImageClick;
    tmpImage.OnMouseEnter:=OnCloseImageHoverClick;
    tmpImage.OnMouseLeave:=OnCloseImageLeave;
    tmpImage.OnMouseUp:=OnCloseImageMouseUp;
    tmpImage.OnMouseDown:=OnCloseImageMouseDown;
    if CanClose then
    begin
     if Assigned(fCloseImageNormal) then
     begin
      Image2:=ResizeBitmap(fCloseImageNormal, 16, 16);
      tmpImage.Bitmap:=Image2;
      Image2.free;
     end;
    end
    else
      tmpImage.Visible:=false;
  end;

  //icon image
  tmpFMX:=FindStyleResource('iconimage');
  if (tmpFMX is TImage) then
  begin
    tmpImage:=TImage(tmpFMX);
    if Assigned(fIcon) then
      tmpImage.Bitmap:=fIcon;
    if fShowIcon then
      tmpImage.Visible:=true
    else
      tmpImage.Visible:=false;
  end;

end;

constructor TneTabItem.Create(AOwner: TComponent);
begin
  inherited;

  fCanClose:=True;
  fTabControlParent:=AOwner as TTabControl;

  Self.OnMouseEnter:=NewOnMouseEnter;
  Self.OnMouseLeave:=NewOnMouseLeave;
  Self.OnMouseMove:=NewOnMouseMove;

  Self.StyleLookup:='netabitemstyle';
  CloseImageNormal:=nil;
  CloseImageHover:=nil;
  CloseImagePressed:=nil;
  ShowIcon:=true;

end;


procedure TneTabItem.DblClick;
begin
  inherited;
  if Assigned(fTabControlParent) and (fTabControlParent is TneTabControl) then
    (fTabControlParent as TneTabControl).DblClick;
end;

function TneTabItem.GetVersion: string;
begin
  result:=MajorVersion+'.'+MinorVersion+'.'+BugVersion;
end;

procedure TneTabItem.MouseClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  tmp: TneTabControl;
begin
  inherited;

  if Assigned(fTabControlParent) and (fTabControlParent is TneTabControl) then
    tmp:=fTabControlParent as TNETabControl
  else
    tmp:=nil;

  if not Assigned(tmp) then
    Exit;

  if Button=TMouseButton.mbRight then
    tmp.ShowPopUpMenu(self,X,Y);
end;

procedure TneTabItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  fIsMousePressed:=true;
end;

procedure TneTabItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  fIsMousePressed:=false;
end;

procedure TneTabItem.OnCloseImageClick(ASender: TObject);
begin
  if Assigned(fTabControlParent) and (fTabControlParent is TneTabControl) then
    (fTabControlParent as TneTabControl).DeleteTab(self.TagString);
end;

procedure TneTabItem.OnCloseImageHoverClick(ASender: TObject);
var
  tmpFMX: TFMXObject;
  tmpImage: TImage;
  Image2: TBitmap;
begin
  inherited;
  //close image
  tmpFMX:=FindStyleResource('closeimage');
  if (tmpFMX is TImage) then
  begin
    tmpImage:=TImage(tmpFMX);
    if CanClose then
    begin
     if Assigned(fCloseImageHover) then
     begin
      Image2:=ResizeBitmap(fCloseImageHover, 16, 16);
      tmpImage.Bitmap:=Image2;
      Image2.Free;
     end;
    end
end;
end;

procedure TneTabItem.OnCloseImageLeave(ASender: TObject);
begin
  inherited;
  ApplyStyle;
end;

procedure TneTabItem.OnCloseImageMouseDown(ASender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  tmpFMX: TFMXObject;
  tmpImage: TImage;
  Image2: TBitmap;
begin
  inherited;
  //close image
  tmpFMX:=FindStyleResource('closeimage');
  if (tmpFMX is TImage) then
  begin
    tmpImage:=TImage(tmpFMX);
    if CanClose then
    begin
     if Assigned(fCloseImagePressed) then
     begin
      Image2:=ResizeBitmap(fCloseImagePressed, 16, 16);
      tmpImage.Bitmap:=Image2;
      Image2.Free;
     end;
    end
end;
end;

procedure TneTabItem.OnCloseImageMouseUp(ASender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  OnCloseImageHoverClick(ASender);
end;

procedure TneTabItem.NewOnMouseEnter(ASender: TObject);
begin
  inherited;
  if Assigned(fTabControlParent) and (fTabControlParent is TneTabControl) then
    (fTabControlParent as TneTabControl).MouseEnterTabItem(ASender);
end;

procedure TneTabItem.NewOnMouseLeave(ASender: TObject);
begin
  inherited;
  if Assigned(fTabControlParent) and (fTabControlParent is TneTabControl) then
    (fTabControlParent as TneTabControl).MouseExitTabItem;
  ApplyStyle;
end;

procedure TneTabItem.NewOnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
 if Assigned(fTabControlParent) and (fTabControlParent is TneTabControl) then
    (fTabControlParent as TneTabControl).MouseMoveOverTabItem
      (self.TagString, X, Y);
  ApplyStyle;
end;

procedure TneTabItem.SetCanClose(const can: Boolean);
begin
  fCanClose:=can;
  SetCloseImageNormal(fCloseImageNormal);
end;

procedure TneTabItem.SetCloseImageHover(const newImage: TBitmap);
begin
  fCloseImageHover:=newImage;
  ApplyStyle;
end;

procedure TneTabItem.SetCloseImageNormal(const newImage: TBitmap);
begin
  fCloseImageNormal:=newImage;
  ApplyStyle;
end;


procedure TneTabItem.SetCloseImagePressed(const newImage: TBitMap);
begin
  fCloseImagePressed:=newImage;
  ApplyStyle;
end;

procedure TneTabItem.SetIcon(const newImage: TBitmap);
begin
  fIcon:=newImage;
  ApplyStyle;
end;

procedure TneTabItem.SetShowIcon(const can: Boolean);
begin
  fShowIcon:=can;
  ApplyStyle;
end;

{ THintPanel }

procedure THintPanel.ApplyStyle;
var
  tmpFMX: TFmxObject;
begin
  inherited;
  if not (fHintStyle=THintStyle.ShowTitle) then Exit;
  tmpFMX:=FindStyleResource('hinttext');
  if (tmpFMX is TLabel) then
    (tmpFMX as TLabel).Text:=fHintText;
end;

procedure THintPanel.SetHintStyle(const newHintStyle: THintStyle);
begin
  fHintStyle:=newHintStyle;
  if fHintStyle=THintStyle.ShowTitle then
    Self.StyleLookup:='nehintstyle'
  else
    self.StyleLookup:='';
  ApplyStyle;
end;

procedure THintPanel.SetHintText(const newHintText: string);
begin
  fHintText:=newHintText;
  ApplyStyle;
end;

{ functions }

function FindParentNeTabItem(const checkComponent: TFMXObject):TneTabItem;
begin
  if not Assigned(checkComponent.Parent) then
    result:=nil
  else
   if checkComponent.Parent Is TneTabItem then
   begin
     result:=checkComponent.Parent as TneTabItem;
     if (result.TabControlParent is TneTabControl) then
       result:=(result.TabControlParent as TneTabControl).GetTab(result.TagString);
   end
   else
     result:=FindParentNeTabItem(checkComponent.Parent);
end;

end.
