unit ui_ingame_menu;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avCanvas, avMiniControls, mutils;

type

  { TavmMenuBtn }

  TavmMenuBtn = class(TavmCustomButton)
  protected
    procedure DoValidate; override;
  end;

  { TavmInGameMenu }

  TavmInGameMenu = class(TavmCustomControl)
  private
    FbtnResume : TavmMenuBtn;
    FbtnNewGame: TavmMenuBtn;
    FbtnExit   : TavmMenuBtn;
    function GetOnExit: TNotifyEvent;
    function GetOnNewGame: TNotifyEvent;
    function GetOnResume: TNotifyEvent;
    procedure SetOnExit(const AValue: TNotifyEvent);
    procedure SetOnNewGame(const AValue: TNotifyEvent);
    procedure SetOnResume(const AValue: TNotifyEvent);
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
  public
    property OnResume : TNotifyEvent read GetOnResume  write SetOnResume;
    property OnNewGame: TNotifyEvent read GetOnNewGame write SetOnNewGame;
    property OnExit   : TNotifyEvent read GetOnExit    write SetOnExit;
  end;

implementation

{ TavmInGameMenu }

function TavmInGameMenu.GetOnResume: TNotifyEvent;
begin
  Result := FbtnResume.OnClick;
end;

procedure TavmInGameMenu.SetOnExit(const AValue: TNotifyEvent);
begin
  FbtnExit.OnClick := AValue;
end;

function TavmInGameMenu.GetOnNewGame: TNotifyEvent;
begin
  Result := FbtnNewGame.OnClick;
end;

function TavmInGameMenu.GetOnExit: TNotifyEvent;
begin
  Result := FbtnExit.OnClick;
end;

procedure TavmInGameMenu.SetOnNewGame(const AValue: TNotifyEvent);
begin
  FbtnNewGame.OnClick := AValue;
end;

procedure TavmInGameMenu.SetOnResume(const AValue: TNotifyEvent);
begin
  FbtnResume.OnClick := AValue;
end;

procedure TavmInGameMenu.AfterRegister;
begin
  inherited AfterRegister;
  Origin := Vec(0.5, 0.5);
  Size := Vec(200, 300);

  FbtnResume := TavmMenuBtn.Create(Self);
  FbtnResume.Origin := Vec(0.5, 0.5);
  FbtnResume.Pos := Vec(Size.x * 0.5, Round(1 * Size.y * 0.25));
  FbtnResume.Size := Vec(160, 64);
  FbtnResume.Text := 'Продолжить';

  FbtnNewGame := TavmMenuBtn.Create(Self);
  FbtnNewGame.Origin := Vec(0.5, 0.5);
  FbtnNewGame.Pos := Vec(Size.x * 0.5, Round(2 * Size.y * 0.25));
  FbtnNewGame.Size := Vec(160, 64);
  FbtnNewGame.Text := 'Новая игра';

  FbtnExit    := TavmMenuBtn.Create(Self);
  FbtnExit.Origin := Vec(0.5, 0.5);
  FbtnExit.Pos := Vec(Size.x * 0.5, Round(3 * Size.y * 0.25));
  FbtnExit.Size := Vec(160, 64);
  FbtnExit.Text := 'Выход';
end;

procedure TavmInGameMenu.DoValidate;
var
  bkColor: TVec4;
begin
  inherited DoValidate;

  bkColor := Vec(0.2, 0.2, 1.0, 1.0);

  Canvas.Clear;
  Canvas.Brush.Color := bkColor;
  Canvas.AddFill(Vec(0,0), Size);
  Canvas.AddRectangle(Vec(0,0), Size);
end;

{ TavmMenuBtn }

procedure TavmMenuBtn.DoValidate;
var bkColor: TVec4;
    txt: ITextLines;
begin
  inherited DoValidate;

  Canvas.Font.Size := 32;

  if Downed then
    bkColor := Vec(0.2, 0.2, 1.0, 1.0)
  else
    if Moved then
      bkColor := Vec(0.4, 0.4, 1.0, 1.0)
    else
      bkColor := Vec(0.3, 0.3, 1.0, 1.0);

  Canvas.Clear;
  Canvas.Brush.Color := bkColor;
  Canvas.AddFill(Vec(0,0), Size);
  Canvas.AddRectangle(Vec(0,0), Size);

  with Canvas.TextBuilder do
  begin
    Align := TLineAlign.laCenter;
    WriteLn(Text);
    txt := Finish();
    txt.BoundsX := Vec(0, Size.x);
    txt.BoundsY := Vec(0, Size.y);
    txt.VAlign := 0.5;
    Canvas.AddText(txt);
  end;
end;

end.

