This unit provides a `TYookassaWebhookHandler` class that can be easily integrated into any web application, regardless of the framework used. 

All the examples use the same approach: 

  *  `POST` request processing
  *  Passing the request body to `TYookassaWebhookHandler.HandleWebhook`
  *  Enabling event handlers
  *  Logging (optional)

# FCL-web (`TFPHTTPServer`)

```pascal
uses
  fphttpserver, fphttp, yookassa_webhook;

type
  { TMyWebServer }
  TMyWebServer = class
  private
    FServer: TFPHTTPServer;
    FWebhookHandler: TYookassaWebhookHandler;
    procedure HandleWebhook(ARequest: TFPHTTPConnectionRequest; AResponse: TFPHTTPConnectionResponse);
    procedure OnPaymentSucceeded(const AEvent: TYookassaWebhookData);
    procedure OnLog(AEvent: TEventType; const AMsg: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
  end;

{ TMyWebServer }

constructor TMyWebServer.Create;
begin
  FServer := TFPHTTPServer.Create(nil);
  FWebhookHandler := TYookassaWebhookHandler.Create('unused');
  FWebhookHandler.OnPaymentSucceeded := @OnPaymentSucceeded;
  FWebhookHandler.OnLog := @OnLog;
  FServer.OnRequest := @HandleWebhook;
end;

destructor TMyWebServer.Destroy;
begin
  FWebhookHandler.Free;
  FServer.Free;
  inherited Destroy;
end;

procedure TMyWebServer.HandleWebhook(ARequest: TFPHTTPConnectionRequest; AResponse: TFPHTTPConnectionResponse);
var
  aBody: string;
begin
  if (ARequest.Method = 'POST') and (ARequest.URI = '/webhook/yookassa') then
  begin
    aBody := ARequest.Content;
    AResponse.Content := FWebhookHandler.HandleWebhook(aBody);
    AResponse.ContentType := 'application/json';
    AResponse.StatusCode := 200;
  end
  else
  begin
    AResponse.StatusCode := 404;
    AResponse.ContentText := 'Not Found';
  end;
end;

procedure TMyWebServer.OnPaymentSucceeded(const AEvent: TYookassaWebhookData);
begin
  // Your business logic: updating order status, sending email, etc.
  WriteLn('Платёж ', AEvent.ObjectId, ' успешно оплачен!');
end;

procedure TMyWebServer.OnLog(AEvent: TEventType; const AMsg: string);
begin
  WriteLn('[YooKassa Webhook] ', AMsg);
end;

procedure TMyWebServer.Start;
begin
  FServer.Port := 8080;
  FServer.Active := True;
  WriteLn('Сервер запущен на http://localhost:8080');
end;

var
  aServer: TMyWebServer;
begin
  aServer := TMyWebServer.Create;
  try
    aServer.Start;
    ReadLn; // Await for finish
  finally
    aServer.Free;
  end;
end.
```

# BrookFreePascal (`TBrookAction`)

```pascal

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BrookAction, yookassa_webhook
  ;  

type

  { TYooKassaAction }

  TYooKassaAction = class(TBrookAction)
  private
    FHandler: TYookassaWebhookHandler;
    procedure OnPaymentSucceeded(const AEvent: TYookassaWebhookData);
    procedure OnLog(AEvent: TEventType; const AMsg: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Post; override;
  end;

implementation

{ TYooKassaAction }

constructor TYooKassaAction.Create;
begin
  inherited Create;
  FHandler := TYookassaWebhookHandler.Create(EmptyStr);
  FHandler.OnPaymentSucceeded := @OnPaymentSucceeded;
  FHandler.OnLog := @OnLog;
end;

destructor TYooKassaAction.Destroy;
begin
  FHandler.Free;
  inherited Destroy;
end;

procedure TYooKassaAction.Post;
var
  aBody: string;
begin
  aBody := HttpRequest.Content;
  HttpResponse.ContentType:='application/json';
  HttpResponse.Content:=FHandler.HandleWebhook(aBody);
  HttpResponse.Code:=200;
end;

procedure TYooKassaAction.OnPaymentSucceeded(const AEvent: TYookassaWebhookData);
begin
  // Update DB, send notification
  WriteLn('BrookFP: Платёж ', AEvent.ObjectId, ' завершён успешно.');
end;

procedure TYooKassaAction.OnLog(AEvent: TEventType; const AMsg: string);
begin
  WriteLn('[BrookFP Webhook] ', AMsg);
end;

initialization
  TYooKassaAction.Register('/some_url_with_some_secrettoken');    

end.
```

## Safety recommendations 

    Use a secret URL, for example: `/webhook/yookassa/your-secret-token-here`
    Do not use the SecretKey from the API in `TYookassaWebhookHandler` — it is not needed.
    Restrict access by IP, if possible
    Always log suspicious requests.