{***************************************************************
 * Name:      VideoTest
 * Author:    Denis Gottardello (info@denisgottardello.it)
 * Created:   2010-08-12
 * Copyright: Denis Gottardello (www.denisgottardello.it)
 * License:
 **************************************************************}

{****************************************************************
 * Ajustes Win/Linux por Anderson Junior (andersonscinfo@gmail.com)
 * Obs:
}
unit TFMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Buttons, StdCtrls, ExtCtrls,
  {$ifdef Windows}
  TFCamera,
  VFW,
  {$endif}
  {$ifdef unix}
  videodev2, gtk2, gdk2, gdk2x, glib,
  {$endif}
  FileUtil
  ;

{$ifdef unix}
const
  VIDEO_BUFFERS= 4;

type

  ThCamera = class(TThread)
  private
  protected
    procedure Execute; override;
  public
    UscireSubito: Boolean;
    constructor Create();
    destructor Destroy(); override;
  end;

{$endif}

{$ifdef Windows}
type

  RVideoCapDevice= record
    szDeviceName: array[0..79] of Char;
    szDeviceVersion: array[0..79] of Char;
  end;
{$endif}

type AImageIn= array of Byte;
type PAImageIn= ^AImageIn;
{$ifdef Windows}
type Av_capability= array of RVideoCapDevice;
{$endif}
{$ifdef unix}
type Av_capability= array of v4l2_capability;
{$endif}

type

  { TMainForm }

  TMainForm = class(TForm)
    BitBtn1: TBitBtn;
    TBBCamera: TBitBtn;
    GroupBox2: TGroupBox;
    TMLog: TMemo;
    TEDriver: TEdit;
    TECard: TEdit;
    TEBusInfo: TEdit;
    TEVersion: TEdit;
    TECapabilities: TEdit;
    TEReserved: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    TLBVideoCards: TListBox;
    StatusBar1: TStatusBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TBBCameraClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TLBVideoCardsClick(Sender: TObject);
  private
    AAv_capability: Av_capability;
    TSLInterfacesList: TStringList;
    IsCameraConnected: Boolean;
    FHandle: Integer;

    {$ifdef Windows}
    Camera: TCamera;
    {$endif}
    {$ifdef unix}
    vbuf: v4l2_buffer;
    FWidth, FHeight, CurrentVideoFormat: Integer;
    reqbuf: v4l2_requestbuffers;
    VideoBuffer: array[0..VIDEO_BUFFERS- 1] of pbyte;
    TThCamera: TObject;
    GrabImage: Boolean;
    FWindow, drawingarea: PGtkWidget;
    FImage: PGdkImage;
    TDTMoment: TDateTime;
    Frames: LongInt;

    function Clip(Valore: Integer): Integer;
    function GetData: Pointer;
    function IntArrayToString(PInt: PInteger): String;
    procedure GetResolution(Larghezza, Altezza: PInteger);
    procedure yuv2ToData(Src, Dest: PLongWord; Size: Integer);
    procedure RGB24ToData(Src, Dest: PLongWord);
    property Data: Pointer read GetData;

    procedure CloseCamera();
    function OpenCamera(): Boolean;
    procedure GetFrame();
    {$endif}

  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

{$ifdef unix}
function CloseVideo(widget: pGtkWidget; event: pGdkEvent; data: gpointer): gint; cdecl;
{$endif}

implementation

uses
  {$ifdef Windows}
  Windows,
  {$endif}
  {$ifdef unix}
  BaseUnix, RGBGraphics,
  {$endif}
  FPWriteJPEG,
  DateUtils
  ;

{$ifdef unix}
{ThCamera}
constructor ThCamera.Create();
begin
  self.FreeOnTerminate:= false;
  self.Priority:= tpNormal;
  UscireSubito:= false;
  inherited Create(false);
end;

Destructor ThCamera.Destroy();
begin
  inherited Destroy();
end;

procedure ThCamera.Execute;
begin
  if MainForm.OpenCamera() then begin
    while not Terminated and not UscireSubito do begin
      Synchronize(@MainForm.GetFrame);
      Sleep(10);
      //Terminate;
    end;
  end;
  MainForm.CloseCamera();
end;

{$endif}

{ TMainForm }

{$ifdef unix}
function CloseVideo(widget: pGtkWidget; event: pGdkEvent; data: gpointer): gint; cdecl;
begin
  CloseVideo:= 0;
  MainForm.TBBCameraClick(nil);
end;
{$endif}


procedure TMainForm.FormShow(Sender: TObject);
var
  {$ifdef windows}
  VideoCapDevice: RVideoCapDevice;
  {$endif}

  {$ifdef unix}
  Rv4l2_capability: v4l2_capability;
  {$endif}
  I: Integer;
begin
  SetLength(AAv_capability, 0);
  for I:= 0 to 99 do
  begin
    {$ifdef windows}
    if capGetDriverDescription(I, @VideoCapDevice.szDeviceName, sizeof(VideoCapDevice.szDeviceName), @VideoCapDevice.szDeviceVersion, sizeof(VideoCapDevice.szDeviceVersion)) then
    begin
      SetLength(AAv_capability, Length(AAv_capability)+ 1);
      AAv_capability[Length(AAv_capability)- 1]:= VideoCapDevice;
      TLBVideoCards.Items.Append(VideoCapDevice.szDeviceName +', '+ VideoCapDevice.szDeviceVersion);
    end;
    {$endif}

    {$ifdef unix}
    if FileExists('/dev/video'+ IntToStr(I)) then
    begin
      FHandle:= FpOpen(PChar('/dev/video'+ IntToStr(I)), O_RDONLY);
      if FpIOCtl(FHandle, VIDIOC_QUERYCAP, @Rv4l2_capability)= 0 then
      begin
        SetLength(AAv_capability, Length(AAv_capability)+ 1);
        AAv_capability[Length(AAv_capability)- 1]:= Rv4l2_capability;
        TLBVideoCards.Items.Append(StrPas(@Rv4l2_capability.card));
        TSLInterfacesList.Append('/dev/video'+ IntToStr(I));
      end;
      FpClose(FHandle);
    end;
    {$endif}
  end;

end;


procedure TMainForm.TBBCameraClick(Sender: TObject);

var
  {$ifdef windows}
  FDriverCaps: TCapDriverCaps;
  {$endif}

  {$ifdef unix}
  vbox, bclose, frame, topbox: PGtkWidget;
  {$endif}
begin
  {$ifdef windows}
  if not IsCameraConnected then begin
    Camera:= TCamera.Create(Self);
    Camera.FHandle:= @FHandle;
    FHandle:= capCreateCaptureWindow(nil, WS_CHILDWINDOW or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS , Camera.TPCamera.Left+ 5, Camera.TPCamera.Top+ 5, Camera.TPCamera.Width- 10, Camera.TPCamera.Height-10, Camera.TPCamera.Handle, 0);
    if (FHandle<> 0) and capDriverConnect(FHandle, TLBVideoCards.ItemIndex) then begin
      if capDriverGetCaps(FHandle, @FDriverCaps, SizeOf(TCapDriverCaps)) then begin
        if FDriverCaps.fHasOverlay then begin
          TMLog.Lines.Append('Driver connected, accepts overlay');
          if not capPreviewRate(FHandle, 0) then TMLog.Lines.Append('Error capPreviewRate(FHandle, 0)!');
          if capOverlay(FHandle, True) then TMLog.Lines.Append('Video Capture - Overlay (Hardware)');
        end else begin
          TMLog.Lines.Append('Driver connected, software rendering');
          if not capPreviewRate(FHandle, 33) then TMLog.Lines.Append('Error capPreviewRate(FHandle, 33)!');
          if capPreview(FHandle, True) then TMLog.Lines.Append('Video Capture - Preview (Software)');
        end;
        IsCameraConnected:= true;
        TBBCamera.Caption:= 'Stop';
      end;
    end;
    Camera.Show();
  end else begin
    if capCaptureStop(FHandle) and capDriverDisconnect(FHandle) then begin
      Camera.Free;
      IsCameraConnected:= false;
      TBBCamera.Caption:= 'Start';
    end;
  end;
  {$endif}

  {$ifdef unix}
  GetResolution(@FWidth, @FHeight);
    if TThCamera= nil then begin
      //TCBResolution.Enabled:= false;
      FWindow:= gtk_window_new(GTK_WINDOW_TOPLEVEL);
      gtk_window_set_title(GTK_WINDOW (FWindow), PChar(TLBVideoCards.Items[TLBVideoCards.ItemIndex]));
      gtk_window_set_resizable(GTK_WINDOW(FWindow), TRUE);

      topbox:= gtk_vbox_new(FALSE, 0);
      gtk_container_add(GTK_CONTAINER(FWindow), topbox);
      frame:= gtk_frame_new(nil);
      gtk_box_pack_start(GTK_BOX(topbox), frame, FALSE, TRUE, 0);
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
      drawingarea:= gtk_drawing_area_new();
      gtk_drawing_area_size(GTK_DRAWING_AREA(drawingarea), FWidth, FHeight);
      gtk_container_add(GTK_CONTAINER(frame), drawingarea);

      vbox:= gtk_hbox_new(FALSE, 5);
      bclose:= gtk_button_new_with_label('Close');
      gtk_signal_connect(GTK_OBJECT(bclose), 'clicked', GTK_SIGNAL_FUNC(@CloseVideo), nil);
      gtk_box_pack_start(GTK_BOX(vbox), bclose, FALSE, FALSE, 0);
      gtk_box_pack_start(GTK_BOX(topbox), vbox, FALSE, TRUE, 0);

      gtk_widget_show(bclose);
      gtk_widget_show(vbox);
      gtk_widget_show(drawingarea);
      gtk_widget_show(frame);
      gtk_widget_show(topbox);
      gtk_widget_show(FWindow);

      FImage:= gdk_image_new(GDK_IMAGE_NORMAL, gtk_widget_get_visual(FWindow), FWidth, FHeight);
      if FImage= nil then TMLog.Lines.Append('Could not create gtk image!');
      TBBCamera.Caption:= 'Stop';
      TThCamera:= ThCamera.Create();
      TDTMoment:= Now;
      Frames:= 0;
    end else begin
      TMLog.Lines.Append('frames / second: '+ IntToStr(Frames div SecondsBetween(TDTMoment, Now)));
      ThCamera(TThCamera).UscireSubito:= true;
      ThCamera(TThCamera).Terminate;
      ThCamera(TThCamera).WaitFor;
      ThCamera(TThCamera).Free;
      ThCamera(TThCamera):= nil;
      if FWindow<> nil then begin
        gtk_widget_hide(FWindow);
        gtk_widget_destroy(FWindow);
        FWindow:= nil;
      end;
      TBBCamera.Caption:= 'Start';
      //TCBResolution.Enabled:= true;
    end;
  {$endif}

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TSLInterfacesList:= TStringList.Create();
  IsCameraConnected:= false;

  {$ifdef unix}
  TThCamera:= nil;
  GrabImage:= false;
  {$endif}
end;

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin
  {$ifdef unix}
  if TThCamera<> nil then
    TBBCameraClick(Sender);
  {$endif}
  Close();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  TSLInterfacesList.Free;
end;

procedure TMainForm.TLBVideoCardsClick(Sender: TObject);
begin
  {$ifdef windows}
  if (TLBVideoCards.ItemIndex> -1) and (TLBVideoCards.ItemIndex< TLBVideoCards.Items.Count) then begin
    TEDriver.Text:= AAv_capability[TLBVideoCards.ItemIndex].szDeviceName;
    TEVersion.Text:= AAv_capability[TLBVideoCards.ItemIndex].szDeviceVersion;
    TBBCamera.Enabled:= true;
  end;
  {$endif}

  {$ifdef unix}
  if (TLBVideoCards.ItemIndex> -1) and (TLBVideoCards.ItemIndex< TLBVideoCards.Items.Count) then begin
    TEDriver.Text:= StrPas(@AAv_capability[TLBVideoCards.ItemIndex].driver);
    TECard.Text:= StrPas(@AAv_capability[TLBVideoCards.ItemIndex].card);
    TEBusInfo.Text:= StrPas(@AAv_capability[TLBVideoCards.ItemIndex].bus_info);
    TEVersion.Text:= IntToStr(AAv_capability[TLBVideoCards.ItemIndex].version);
    TECapabilities.Text:= IntToStr(AAv_capability[TLBVideoCards.ItemIndex].capabilities);
    TEReserved.Text:= IntArrayToString(@AAv_capability[TLBVideoCards.ItemIndex].reserved);
    //TBBInfo.Enabled:= true;
    TBBCamera.Enabled:= true;
  end;
  {$endif}
end;

{$ifdef unix}
function TMainForm.Clip(Valore: Integer): Integer;
begin
  if Valore> 255 then Clip:= 255
  else if Valore< 0 then Clip:= 0
  else Clip:= Valore;
end;

function TMainForm.GetData: Pointer;
begin
  Result:= FImage^.mem;
end;

function TMainForm.IntArrayToString(PInt: PInteger): String;
var
  I: Integer;
begin
  IntArrayToString:= '';
  for I:= 0 to sizeof(PInt)- 1 do IntArrayToString:= IntArrayToString+ IntToStr(PInt[I])+ '.';
  if Length(IntArrayToString)> 0 then IntArrayToString:= Copy(IntArrayToString, 1, Length(IntArrayToString)- 1);
end;

procedure TMainForm.GetResolution(Larghezza, Altezza: PInteger);
begin
  Larghezza^:= 640;
  Altezza^:= 480;
  {
  case TCBResolution.ItemIndex of
    0: begin
      Larghezza^:= 320;
      Altezza^:= 240;
    end;
    1: begin
      Larghezza^:= 640;
      Altezza^:= 480;
    end;
    2: begin
      Larghezza^:= 800;
      Altezza^:= 600;
    end;
  end;   }
end;

procedure TMainForm.yuv2ToData(Src, Dest: PLongWord; Size: Integer);
// http://en.wikipedia.org/wiki/YUV
var
  I: Integer;
  R, G, B, U, Y1, V, Y2: Integer;
begin
  for I:= 0 to (size div 2)- 1 do begin
    U:= Src^ shr 24;
    Y1:= (Src^ shr 16) and $ff;
    V:= (Src^ shr 8) and $ff;
    Y2:= Src^ and $ff;

    R:= Clip((9535 * (Y1 - 16) + 13074 * (V - 128)) shr 13);
    G:= Clip((9535 * (Y1 - 16) - 6660 * (V - 128) - 3203 * (U - 128)) shr 13);
    B:= Clip((9535 * (Y1 - 16) + 16531 * (U - 128)) shr 13);
    Dest^:= (R shl 16) or (G shl 8) or B;
    Inc(Dest);

    R:= Clip((9535 * (Y2 - 16) + 13074 * (V - 128)) shr 13);
    G:= Clip((9535 * (Y2 - 16) - 6660 * (V - 128) - 3203 * (U - 128)) shr 13);
    B:= Clip((9535 * (Y2 - 16) + 16531 * (U - 128)) shr 13);
    Dest^:= (R shl 16) or (G shl 8) or B;
    Inc(Src);
    Inc(Dest);
  end;
end;

procedure TMainForm.RGB24ToData(Src, Dest: PLongWord);
var
  I: LongWord;
begin
  I:= 0;
  while I< vbuf.bytesused do begin
    Dest^:= (VideoBuffer[vbuf.index][I] shl 16) + (VideoBuffer[vbuf.index][I+ 1] shl 8) + VideoBuffer[vbuf.index][I+ 2];
    Inc(I, 3);
    Inc(Dest);
  end;
end;



procedure TMainForm.CloseCamera();
var
  I: Integer;
begin
  try
    // stop streaming
    I:= vbuf._type;
    if FpIOCtl(FHandle, VIDIOC_STREAMOFF, @I)< 0 then TMLog.Lines.Append('Could not stop streaming');
    // unmmap and unqueue the buffers
    for I:= 0 to reqbuf.count- 1 do if VideoBuffer[I]<> MAP_FAILED then FpMunMap(VideoBuffer[I], vbuf.length);
    ///
    FpClose(FHandle);
    Except on e: Exception do begin
      TMLog.Lines.Append(e.Message);
    end;
  end;
end;

function TMainForm.OpenCamera(): Boolean;
var
  fmt: v4l2_format;
  I: Integer;
begin
  OpenCamera:= false;
  try
    FHandle:= FpOpen(PChar(TSLInterfacesList[TLBVideoCards.ItemIndex]), O_RDWR);
    if FHandle> -1 then begin
      // set video input
      I:= 0;//TSEInput.Value;
      if FpIOCtl(FHandle, VIDIOC_S_INPUT, @I)< 0 then TMLog.Lines.Append('Could not set video input') else begin
        // set video format
        CurrentVideoFormat:= 0;
        while true do begin
          FillChar(fmt, sizeof(v4l2_format), 0);
          fmt._type:= V4L2_BUF_TYPE_VIDEO_CAPTURE;
          GetResolution(@fmt.fmt.pix.width, @fmt.fmt.pix.height);
          fmt.fmt.pix.pixelformat:= V4L2_PIX_FMT_JPEG;
          fmt.fmt.pix.field:= V4L2_FIELD_ANY;
          if FpIOCtl(FHandle, VIDIOC_S_FMT, @fmt)> -1 then begin
            CurrentVideoFormat:= V4L2_PIX_FMT_JPEG;
            TMLog.Lines.Append('Current video format is V4L2_PIX_FMT_JPEG');
            break;
          end;
          if CurrentVideoFormat= 0 then begin
            FillChar(fmt, sizeof(v4l2_format), 0);
            fmt._type:= V4L2_BUF_TYPE_VIDEO_CAPTURE;
            GetResolution(@fmt.fmt.pix.width, @fmt.fmt.pix.height);
            fmt.fmt.pix.pixelformat:= V4L2_PIX_FMT_RGB24;
            fmt.fmt.pix.field:= V4L2_FIELD_ANY;
            if FpIOCtl(FHandle, VIDIOC_S_FMT, @fmt)> -1 then begin
              CurrentVideoFormat:= V4L2_PIX_FMT_RGB24;
              TMLog.Lines.Append('Current video format is V4L2_PIX_FMT_RGB24');
              break;
            end;
          end;
          if CurrentVideoFormat= 0 then begin
            FillChar(fmt, sizeof(v4l2_format), 0);
            fmt._type:= V4L2_BUF_TYPE_VIDEO_CAPTURE;
            GetResolution(@fmt.fmt.pix.width, @fmt.fmt.pix.height);
            fmt.fmt.pix.pixelformat:= V4L2_PIX_FMT_YUYV;
            fmt.fmt.pix.field:= V4L2_FIELD_ANY;
            if FpIOCtl(FHandle, VIDIOC_S_FMT, @fmt)> -1 then begin
              CurrentVideoFormat:= V4L2_PIX_FMT_YUYV;
              TMLog.Lines.Append('Current video format is V4L2_PIX_FMT_YUYV');
              break;
            end;
          end;
        end;
        if CurrentVideoFormat<> 0 then begin
          // set up buffers
          FillChar(reqbuf, sizeof(v4l2_requestbuffers), 0);
          reqbuf.count:= VIDEO_BUFFERS;
          reqbuf._type:= V4L2_BUF_TYPE_VIDEO_CAPTURE;
          reqbuf.memory:= V4L2_MEMORY_MMAP;
          if FpIOCtl(FHandle, VIDIOC_REQBUFS, @reqbuf)= -1 then TMLog.Lines.Append('VIDIOC_REQBUFS error! '+ IntToStr(FpGetErrno())) else begin
            // mmap and queue the buffers into process memory
            for I:= 0 to reqbuf.count- 1 do begin
              FillChar(vbuf, sizeof(v4l2_buffer), 0);
              vbuf.index:= I;
              vbuf._type:= reqbuf._type;
              vbuf.memory:= V4L2_MEMORY_MMAP;
              if FpIOCtl(FHandle, VIDIOC_QUERYBUF, @vbuf)< 0 then TMLog.Lines.Append('Could not queue video buffer') else begin
                VideoBuffer[I]:= FpMMap(nil, vbuf.length, PROT_READ or PROT_WRITE, MAP_SHARED, FHandle, vbuf.m.offset);
                if VideoBuffer[I]= MAP_FAILED then TMLog.Lines.Append('Could not mmap video buffer '+ IntToStr(I)+ ' error: '+ IntToStr(FpGetErrno())) else begin
                  // queue the buffers
                  FillChar(VideoBuffer[I][0], vbuf.length, 0);
                  vbuf.index:= I;
                  vbuf._type:= reqbuf._type;
                  vbuf.memory:= V4L2_MEMORY_MMAP;
                  vbuf.input:= 1;
                  if FpIOCtl(FHandle, VIDIOC_QBUF, @vbuf)<> 0 then TMLog.Lines.Append('Could not queue video buffer') else OpenCamera:= true;
                end;
              end;
            end;
            // start streaming
            I:= vbuf._type;
            if FpIOCtl(FHandle, VIDIOC_STREAMON, @I)< 0 then TMLog.Lines.Append('Could not start streaming');
          end;
        end;
      end;
    end else TMLog.Lines.Append('FHandle error!');
    Except on e: Exception do begin
      TMLog.Lines.Append(e.Message);
    end;
  end;
end;

procedure TMainForm.GetFrame();
type
  AFileOut= array of LongWord;
var
  AAImmagineIn: AFileOut;
  I, X, Y: Integer;
  SaveDialog: TSelectDirectoryDialog;
  TRGBImage: TRGB32Bitmap;
begin
  // set video input
  I:= 0;//TSEInput.Value;
  if FpIOCtl(FHandle, VIDIOC_S_INPUT, @I)< 0 then TMLog.Lines.Append('Could not set video input');
  // get the ready to use buffer
  FillChar(vbuf, sizeof(v4l2_buffer), 0);
  vbuf._type:= V4L2_BUF_TYPE_VIDEO_CAPTURE;
  vbuf.memory:= V4L2_MEMORY_MMAP;
  if FpIOCtl(FHandle, VIDIOC_DQBUF, @vbuf)< 0 then TMLog.Lines.Append('VIDIOC_DQBUF error!') else begin
    if vbuf.bytesused> 0 then begin
      if CurrentVideoFormat= V4L2_PIX_FMT_RGB24 then RGB24ToData(PLongWord(VideoBuffer[vbuf.index]), Self.Data)
      else if CurrentVideoFormat= V4L2_PIX_FMT_YUYV then yuv2ToData(PLongWord(VideoBuffer[vbuf.index]), Self.Data, FWidth * FHeight);
      if GrabImage then begin
        GrabImage:= false;
        TRGBImage:= TRGB32Bitmap.Create(FWidth, FHeight);
        SaveDialog:= TSelectDirectoryDialog.Create(Self);
        try
          SetLength(AAImmagineIn, FWidth * FHeight);
          if CurrentVideoFormat= V4L2_PIX_FMT_RGB24 then RGB24ToData(PLongWord(VideoBuffer[vbuf.index]), @AAImmagineIn[0])
          else if CurrentVideoFormat= V4L2_PIX_FMT_YUYV then yuv2ToData(PLongWord(VideoBuffer[vbuf.index]), @AAImmagineIn[0], FWidth * FHeight);
          X:= 0;
          Y:= 0;
          for I:= 0 to Length(AAImmagineIn)- 1 do begin
            TRGBImage.Get32PixelPtr(X, Y)^:= AAImmagineIn[I];
            Inc(X);
            if X>= FWidth then begin
              X:= 0;
              Inc(Y);
            end;
          end;
          if (SaveDialog.Execute()) then TRGBImage.SaveToFile(SaveDialog.FileName+ '/Snap.jpg');
        finally
          TRGBImage.Free;
          SaveDialog.Free;
        end;
      end;
      gdk_draw_image(drawingarea^.window, drawingarea^.style^.white_gc, FImage, 0, 0, 0, 0, FWidth, FHeight);
      Inc(Frames);
    end;
  end;
  // re-submit the buffer
  if FpIOCtl(FHandle, VIDIOC_QBUF, @vbuf) < 0 then TMLog.Lines.Append('VIDIOC_DQBUF error!');
end;
{$endif}

initialization
  {$I TFMainForm.lrs}

end.

