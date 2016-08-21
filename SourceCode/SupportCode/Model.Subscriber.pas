/// <exclude />
unit Model.Subscriber;

interface

uses Model.IntActions, Model.Interf;

function SubscriberClass: ISubscriber;

implementation

type
  TSubscriber = class (TInterfacedObject, ISubscriber)
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
    procedure UpdateSubscriber (action: TIntActions); overload;
    procedure UpdateSubscriber (notificationClass: INotification); overload;
    procedure SetUpdateSubscriberMethod (newMethod: TUpdateSubscriberMethod); overload;
    procedure SetUpdateSubscriberMethod (newNotificationClass: TUpdateSubscriberInterfaceMethod); overload;
  end;


{ TProSuSubscriber }
function SubscriberClass:ISubscriber;
begin
  result:=TSubscriber.Create;
end;


procedure TSubscriber.SetUpdateSubscriberMethod(
  newMethod: TUpdateSubscriberMethod);
begin
  fUpdateMethod:=newMethod;
end;

procedure TSubscriber.UpdateSubscriber(action: TIntActions);
begin
  if Assigned(fUpdateMethod) then fUpdateMethod(action);
end;

procedure TSubscriber.SetUpdateSubscriberMethod(
  newNotificationClass: TUpdateSubscriberInterfaceMethod);
begin
  fUpdateNotificationMethod:=newNotificationClass;
end;

procedure TSubscriber.UpdateSubscriber(
  notificationClass: INotification);
begin
  if Assigned(fUpdateNotificationMethod) and Assigned(notificationClass) then
    fUpdateNotificationMethod(notificationClass);
end;

end.
