unit uRenderStates;

interface

uses uPersistentClasses, uBaseTypes;

type 
  TRenderState = (
    rsBlend, rsClipDistance, rsColorLogicOp, rsCullFace, rsDepthClamp, rsDepthTest,
	rsDither, rsFrameBufferSRGB, rsLineSmooth, rsMultisample, rsPolygonOffsetFill, 
	rsPolygonOffsetLine, rsPolygonOffsetPoint, rsPolygonSmooth, rsPrimitiveRestart, 
	rsPrimitiveRestartFixedIndex, rsRasterizerDiscard, rsSampleAlphaToCoverage,
	rsSampleAlphaToOne, rsSampleCoverage, rsSampleShading, rsSampleMask, rsScissorTest,
	rsStencilTest, rsTextureCubeMapSeamless, rsProgramPointSize
  );
{
  const 
    cGLStates: array[rsBlend..rsProgramPointSize] = (
			GL_BLEND,
			GL_CLIP_DISTANCE,
			GL_COLOR_LOGIC_OP,
			GL_CULL_FACE,
			GL_DEPTH_CLAMP,
			GL_DEPTH_TEST,
			GL_DITHER,
			GL_FRAMEBUFFER_SRGB,
			GL_LINE_SMOOTH,
			GL_MULTISAMPLE,
			GL_POLYGON_OFFSET_FILL,
			GL_POLYGON_OFFSET_LINE,
			GL_POLYGON_OFFSET_POINT,
			GL_POLYGON_SMOOTH,
			GL_PRIMITIVE_RESTART,
			GL_PRIMITIVE_RESTART_FIXED_INDEX,
			GL_RASTERIZER_DISCARD,
			GL_SAMPLE_ALPHA_TO_COVERAGE,
			GL_SAMPLE_ALPHA_TO_ONE,
			GL_SAMPLE_COVERAGE,
			GL_SAMPLE_SHADING,
			GL_SAMPLE_MASK,
			GL_SCISSOR_TEST,
			GL_STENCIL_TEST,
			GL_TEXTURE_CUBE_MAP_SEAMLESS,
			GL_PROGRAM_POINT_SIZE
		);
  
		TGL4xSupportedStates = set of TRenderState = [
			rsBlend, rsCullFace, rsDepthClamp, rsDepthTest, rsScissorTest,
		  rsStencilTest, rsProgramPointSize
		];
		//if not (State in TGL4xSupportedStates) then exit;
}

	TStateManager = class
	private
		class var FStates: set of TRenderState;
		class procedure InitializeStates(aStateManager: TStateManager);
		  //For each of TGLStateManager.SupportedStates do SetState(State, glIsEnable(cGLStates[State]));
		class constructor Create;
		  //FStates := [];		
	public
		class procedure SetState(aState: TRenderState; aValue: boolean); virtual; abstract;
			//if aValue then include(FStates, aState) else exclude(FStates, aState);
		class function GetState(aState: TRenderState): boolean; virtual; abstract;
			//result := aState in FStates;
	end;

