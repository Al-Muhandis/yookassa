program test_yookassa_api_integration_console;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, test_yookassa_api_integration, yookassa_api
  ;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
