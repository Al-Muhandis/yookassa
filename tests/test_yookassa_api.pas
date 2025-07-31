unit test_yookassa_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, yookassa_api
  ;

type
  TTestYooKassa = class(TTestCase)
  published
    procedure TestBuildRequestData;
    procedure TestParseSuccessResponse;
    procedure TestParseErrorResponse;
  end;

implementation

procedure TTestYooKassa.TestBuildRequestData;
var
  aPayment: TYookassaPayment;
  aJSON: String;
begin
  aPayment:=TYookassaPayment.Create;
  try
    aPayment.Amount := 123.45;
    aPayment.Currency := 'RUB';
    aPayment.Description := 'Test payment';
    aJSON := aPayment.BuildPaymentJSON;
  finally
      aPayment.Free;
  end;
  AssertTrue(Pos('"amount"', aJSON) > 0);
  AssertTrue(Pos('"value" : "123.45"', aJSON) > 0);
end;

procedure TTestYooKassa.TestParseSuccessResponse;
var
  aJSON, aConfirmationURL: String;
  aPayment: TYookassaPayment;
begin
  aJSON := '{"id":"pay_123","status":"pending","confirmation":{"type":"redirect","confirmation_url":"https://pay.test"}}';
  aPayment:=TYookassaPayment.Create;
  try
    aConfirmationURL := aPayment.ParseJSONResp(aJSON);
  finally
    aPayment.Free;
  end;
  //AssertEquals('pay_123', Resp.ID);
  //AssertEquals('pending', Resp.Status);
  AssertEquals('https://pay.test', aConfirmationURL);
end;

procedure TTestYooKassa.TestParseErrorResponse;
var
  aJSON: String;
  RaisedError: Boolean;
  aPayment: TYookassaPayment;
begin
  aJSON := '{"type":"error","code":"invalid_request"}';
  RaisedError := False;
  try
    aPayment:=TYookassaPayment.Create;
    try
      aPayment.ParseJSONResp(aJSON);
    finally
      aPayment.Free;
    end;
  except
    on E: Exception do RaisedError := True;
  end;
  AssertTrue('There should be an exception in case of a YooKassa error.', RaisedError);
end;

initialization
  RegisterTest(TTestYooKassa);
end.
