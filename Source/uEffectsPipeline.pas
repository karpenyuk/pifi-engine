unit uEffectsPipeline;

interface

uses uPersistentClasses, uLists, uRenderResource, uBaseTypes;

Type
  TPipelineAbstractEffect = class;

  TEffectNotifyEvent = procedure(Sender: TPipelineAbstractEffect);

  TPipelineAbstractEffect = class (TPersistentResource)
  private
    FShaderProgram: TShaderProgram;
    FonApply: TEffectNotifyEvent;
    FonUnapply: TEffectNotifyEvent;
    FonInitialize: TEffectNotifyEvent;
    FonSetup: TEffectNotifyEvent;
    FUsage: TUsageLogic;
  protected
    FEffectName: string;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Initialize; virtual; abstract;
    procedure Apply; virtual; abstract;
    procedure UnApply; virtual; abstract;
    procedure Setup; virtual; abstract;

    property ShaderProgram: TShaderProgram read FShaderProgram write FShaderProgram;
    property onApply: TEffectNotifyEvent read FonApply write FonApply;
    property onUnapply: TEffectNotifyEvent read FonUnapply write FonUnapply;
    property onInitialize: TEffectNotifyEvent read FonInitialize write FonInitialize;
    property onSetup: TEffectNotifyEvent read FonSetup write FonSetup;
    property EffectName: string read FEffectName;
    property Usage: TUsageLogic read FUsage write FUsage;

  end;

  TBaseEffectList = TDataList<TPipelineAbstractEffect>;

  TMultipassPipelineEffect = class(TPipelineAbstractEffect)

  end;

  TEffectPipeline = class (TPersistentResource)
  private
    FEffectsList: TBaseEffectList;
  end;

implementation

{ TAbstractPipelineEffect }

constructor TPipelineAbstractEffect.Create;
begin
  inherited;
  FShaderProgram:=nil;
  FonApply:=nil;
  FonUnapply:=nil;
  FonInitialize:=nil;
  FonSetup:=nil;
  FEffectName:='AbstractEffect';
  FUsage:=useAlways;
end;

destructor TPipelineAbstractEffect.Destroy;
begin
  inherited;
end;

end.
