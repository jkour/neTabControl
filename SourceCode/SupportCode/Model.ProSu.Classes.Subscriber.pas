/// <exclude />
unit Model.ProSu.Classes.Subscriber;

interface

uses Model.ProSu.InterfaceActions, Model.ProSu.Interfaces;

function SubscriberClass: ISubscriberInterface;

implementation

type
  TProSuSubscriber = class (TInterfacedObject, ISubscriberInterface)
  private
    fUpdateMethod: TUpdateSubscriberMethod;
    fUpdateNotificationMethod: TUpdateSubscriberInterfaceMethod;
  public
    /// <summary>
    ///   Implement this method in the Subscriber class (interface)
    /// </summary>
    /// <param name="action">
    ///   <see cref="Model.ProSu.InterfaceActions|TInterfaceActions" />
    /// </param>
    procedure UpdateSubscriber (action: TInterfaceActions); overload;
    procedure UpdateSubscriber (notificationClass: INotificationInterface); overload;
    procedure SetUpdateSubscriberMethod (newMethod: TUpdateSubscriberMethod); overload;
    procedure SetUpdateSubscriberMethod (newNotificationClass: TUpdateSubscriberInterfaceMethod); overload;
  end;


{ TProSuSubscriber }
function SubscriberClass:ISubscriberInterface;
begin
  result:=TProSuSubscriber.Create;
end;


procedure TProSuSubscriber.SetUpdateSubscriberMethod(
  newMethod: TUpdateSubscriberMethod);
begin
  fUpdateMethod:=newMethod;
end;

procedure TProSuSubscriber.UpdateSubscriber(action: TInterfaceActions);
begin
  if Assigned(fUpdateMethod) then fUpdateMethod(action);
end;

procedure TProSuSubscriber.SetUpdateSubscriberMethod(
  newNotificationClass: TUpdateSubscriberInterfaceMethod);
begin
  fUpdateNotificationMethod:=newNotificationClass;
end;

procedure TProSuSubscriber.UpdateSubscriber(
  notificationClass: INotificationInterface);
begin
  if Assigned(fUpdateNotificationMethod) then
    fUpdateNotificationMethod(notificationClass);
end;

end.
