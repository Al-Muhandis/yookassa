unit test_yookassa_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, yookassa_api
  ;

type

  { TTestYooKassa }

  TTestYooKassa = class(TTestCase)
  published
    procedure TestBuildRequestData;
    procedure TestParseSuccessResponse;
    procedure TestParseErrorResponse;
    procedure TestReceiptItemToJSON;
    procedure TestReceiptToJSON;
  end;

implementation

uses
  fpjson
  ;

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

procedure TTestYooKassa.TestReceiptItemToJSON;
var
  aItem: TYookassaReceiptItem;
  aJSON: TJSONObject;
begin
  aItem := TYookassaReceiptItem.Create;
  try
    aItem.Description := 'Товар';
    aItem.Quantity := 2.0;
    aItem.AmountValue := 150.00;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 1;
    aItem.PaymentMode := 'full_prepayment';
    aItem.PaymentSubject := 'commodity';
    aItem.MarkMode := 0;
    aItem.MarkCodeInfo := 'Base64Code';
    aItem.Measure := 'piece';

    aJSON := aItem.ToJSON;
    try
      AssertEquals('Товар', aJSON.Strings['description']);
      AssertEquals(2.0, aJSON.Floats['quantity']);
      AssertTrue(aJSON.Find('amount') <> nil);
      AssertEquals('RUB', TJSONObject(aJSON.Objects['amount']).Strings['currency']);
      AssertEquals('full_prepayment', aJSON.Strings['payment_mode']);
      AssertEquals('commodity', aJSON.Strings['payment_subject']);
      AssertEquals(0, aJSON.Integers['mark_mode']);
      AssertTrue(aJSON.Find('mark_code_info') <> nil);
      AssertEquals('piece', aJSON.Strings['measure']);
    finally
      aJSON.Free;
    end;
  finally
    aItem.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptToJSON;
var
  aReceipt: TYookassaReceipt;
  aItem: TYookassaReceiptItem;
  aJSON: TJSONObject;
  aItems: TJSONArray;
begin
  aReceipt := TYookassaReceipt.Create;
  try
    aReceipt.CustomerEmail := 'user@example.com';
    aItem := TYookassaReceiptItem.Create;
    aItem.Description := 'Позиция 1';
    aItem.Quantity := 1;
    aItem.AmountValue := 100.00;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 2;
    aReceipt.AddItem(aItem);
    aJSON := aReceipt.ToJSON;
    try
      AssertTrue(aJSON.Find('customer') <> nil);
      AssertEquals('user@example.com', TJSONObject(aJSON.Objects['customer']).Strings['email']);
      AssertTrue(aJSON.Find('items') <> nil);
      aItems := TJSONArray(aJSON.Arrays['items']);
      AssertEquals(1, aItems.Count);
      AssertEquals('Позиция 1', TJSONObject(aItems.Objects[0]).Strings['description']);
      AssertEquals(100.00, TJSONObject(TJSONObject(aItems.Objects[0]).Objects['amount']).Floats['value']);
    finally
      aJSON.Free;
    end;
  finally
    aReceipt.Free;
  end;
end;

initialization
  RegisterTest(TTestYooKassa);
end.
