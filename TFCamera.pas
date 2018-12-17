unit TFCamera;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Graphics, Dialogs,
  ExtCtrls,
  {$ifdef windows}
  Windows,
  {$endif}
  Controls;

type

  { TCamera }

  TCamera = class(TForm)
    TPCamera: TPanel;
    procedure TPCameraResize(Sender: TObject);
  private
    { private declarations }
  public
    FHandle: PInteger;
  end; 

implementation

{ TCamera }

procedure TCamera.TPCameraResize(Sender: TObject);
begin
  {$ifdef windows}
  MoveWindow(FHandle^, 5, 5, TPCamera.Width- 10, TPCamera.Height- 10, False);
  {$endif}
end;

initialization
  {$I TFCamera.lrs}

end.

