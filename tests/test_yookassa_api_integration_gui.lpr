program test_yookassa_api_integration_gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, test_yookassa_api_integration, yookassa_exceptions
  ;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
