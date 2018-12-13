unit ui_start_menu;

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

  { TavmTitle }

  TavmTitle = class(TavmCustomButton)
  private
    procedure AdjustPosition;
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
    procedure Notify_ParentSizeChanged; override;
    procedure Notify_ParentPosChanged; override;
  end;

  { TavmMenu }

  TavmMenu = class(TavmCustomControl)
  private
    procedure AdjustPosition;
  protected
    procedure AfterRegister; override;
    procedure Notify_ParentSizeChanged; override;
    procedure Notify_ParentPosChanged; override;
  end;

  { TavmStartMenu }

  TavmStartMenu = class(TavmMenu)
  private
    FbtnNewGame: TavmMenuBtn;
    FbtnOptions: TavmMenuBtn;
    FbtnExit   : TavmMenuBtn;
    function GetOnExit: TNotifyEvent;
    function GetOnNewGame: TNotifyEvent;
    procedure SetOnExit(const AValue: TNotifyEvent);
    procedure SetOnNewGame(const AValue: TNotifyEvent);
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
  public
    property OnNewGame: TNotifyEvent read GetOnNewGame write SetOnNewGame;
    property OnExit   : TNotifyEvent read GetOnExit    write SetOnExit;
  end;

  { TamvStartTitle }

  TamvStartTitle = class (TavmCustomControl)
  private
    FTitle: TavmTitle;
    FStartMenu: TavmStartMenu;
    function GetOnExit: TNotifyEvent;
    function GetOnNewGame: TNotifyEvent;
    procedure SetOnExit(const AValue: TNotifyEvent);
    procedure SetOnNewGame(const AValue: TNotifyEvent);
  protected
    procedure AfterRegister; override;
    procedure DrawControl(const AMat: TMat3); override;
  public
    property OnNewGame: TNotifyEvent read GetOnNewGame write SetOnNewGame;
    property OnExit   : TNotifyEvent read GetOnExit    write SetOnExit;
  end;

implementation

{ TavmMenu }

procedure TavmMenu.AdjustPosition;
begin
  Pos := Round( (Parent as TavmBaseControl).Size * Vec(0.5, 2/3) );
end;

procedure TavmMenu.AfterRegister;
begin
  inherited AfterRegister;
  Origin := Vec(0.5, 0.5);
end;

procedure TavmMenu.Notify_ParentSizeChanged;
begin
  inherited Notify_ParentSizeChanged;
  AdjustPosition;
end;

procedure TavmMenu.Notify_ParentPosChanged;
begin
  inherited Notify_ParentPosChanged;
  AdjustPosition;
end;

{ TamvStartTitle }

function TamvStartTitle.GetOnExit: TNotifyEvent;
begin
  Result := FStartMenu.OnExit;
end;

function TamvStartTitle.GetOnNewGame: TNotifyEvent;
begin
  Result := FStartMenu.OnNewGame;
end;

procedure TamvStartTitle.SetOnExit(const AValue: TNotifyEvent);
begin
  FStartMenu.OnExit := AValue;
end;

procedure TamvStartTitle.SetOnNewGame(const AValue: TNotifyEvent);
begin
  FStartMenu.OnNewGame := AValue;
end;

procedure TamvStartTitle.AfterRegister;
begin
  inherited AfterRegister;
  FTitle := TavmTitle.Create(Self);

  FStartMenu := TavmStartMenu.Create(Self);
end;

procedure TamvStartTitle.DrawControl(const AMat: TMat3);
begin
  Size := Main.WindowSize;
  inherited DrawControl(AMat);
end;

{ TavmTitle }

procedure TavmTitle.AdjustPosition;
begin
  Pos := Round( (Parent as TavmBaseControl).Size * Vec(0.5, 2/9) );
end;

procedure TavmTitle.AfterRegister;
begin
  inherited AfterRegister;
  Origin := Vec(0.5, 0.5);
  Size := Vec(2, 2);
end;

procedure TavmTitle.DoValidate;
var
  tb: ITextBuilder;
  tl: ITextLines;
begin
  inherited DoValidate;
  Canvas.Clear;
  Canvas.Font.Size := 180;
  Canvas.Font.Color := Vec(1,1,1,1);
  Canvas.Font.Name := 'Constantia';

  tb := Canvas.TextBuilder;
  tb.Align := laCenter;
  tb.Write('Название игры');
  tl := tb.Finish();
  tl.VAlign := 0.5;
  tl.BoundsX := Vec(0,0);
  tl.BoundsY := Vec(0,0);
  Canvas.AddText(tl);
end;

procedure TavmTitle.Notify_ParentSizeChanged;
begin
  inherited Notify_ParentSizeChanged;
  AdjustPosition;
end;

procedure TavmTitle.Notify_ParentPosChanged;
begin
  inherited Notify_ParentPosChanged;
  AdjustPosition;
end;

{ TavmStartMenu }

function TavmStartMenu.GetOnExit: TNotifyEvent;
begin
  Result := FbtnExit.OnClick;
end;

function TavmStartMenu.GetOnNewGame: TNotifyEvent;
begin
  Result := FbtnNewGame.OnClick;
end;

procedure TavmStartMenu.SetOnExit(const AValue: TNotifyEvent);
begin
  FbtnExit.OnClick := AValue;
end;

procedure TavmStartMenu.SetOnNewGame(const AValue: TNotifyEvent);
begin
  FbtnNewGame.OnClick := AValue;
end;

procedure TavmStartMenu.AfterRegister;
var btnSize: TVec2;
begin
  inherited AfterRegister;
  Size := Vec(400, 400);

  btnSize := Vec(350, 100);

  FbtnNewGame := TavmMenuBtn.Create(Self);
  FbtnNewGame.Size := btnSize;
  FbtnNewGame.Text := 'Новая игра';
  FbtnNewGame.Pos := Vec(Size.x*0.5, 30);
  FbtnNewGame.Origin := Vec(0.5, 0.5);

  FbtnOptions := TavmMenuBtn.Create(Self);
  FbtnOptions.Size := btnSize;
  FbtnOptions.Text := 'Настройки';
  FbtnOptions.Pos := Vec(Size.x*0.5, FbtnNewGame.Pos.y + FbtnNewGame.Size.y + 30);
  FbtnOptions.Origin := Vec(0.5, 0.5);

  FbtnExit := TavmMenuBtn.Create(Self);
  FbtnExit.Size := btnSize;
  FbtnExit.Text := 'Выход';
  FbtnExit.Pos := Vec(Size.x*0.5, FbtnOptions.Pos.y + FbtnOptions.Size.y + 30);
  FbtnExit.Origin := Vec(0.5, 0.5);
end;

procedure TavmStartMenu.DoValidate;
begin
  inherited DoValidate;
end;

{ TavmMenuBtn }

procedure TavmMenuBtn.DoValidate;
var bkColor: TVec4;
    txt: ITextLines;
begin
  inherited DoValidate;

  Canvas.Font.Size := 82;

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

