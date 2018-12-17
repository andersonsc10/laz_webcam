program VideoTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, TFMainForm, TFCamera;

{$IFDEF WINDOWS}{$R VideoTest.rc}{$ENDIF}

begin
  {$I VideoTest.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

