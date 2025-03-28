RWByteAddressBuffer Outputs : register(u0);
ByteAddressBuffer Inputs : register(t0);
RWByteAddressBuffer MAGIC : register(u0, space2147420894);

static const uint MagicCodeShift   = 28;
static const uint MagicCodeMask    = 0xf;
static const uint OpcodePhaseShift = 24;
static const uint OpcodePhaseMask  = 0x3;
static const uint DataShift        = 8;
static const uint DataMask         = 0xffff;
static const uint OpcodeShift      = 0;
static const uint OpcodeMask       = 0xff;

static const uint MagicCode = 0x5;

static const uint WaveMatrixMulAcc            = 0x28;
static const uint WaveMatrixUavLoad           = 0x29;
static const uint WaveMatrixUavStore          = 0x2a;
static const uint WaveMatrixGlobalLoad        = 0x2b;
static const uint WaveMatrixGlobalStore       = 0x2c;
static const uint WaveMatrixLdsLoad           = 0x2d;
static const uint WaveMatrixLdsStore          = 0x2e;
static const uint WaveMatrixElementFill       = 0x2f;
static const uint WaveMatrixElementExtract    = 0x30;
static const uint WaveMatrixLength            = 0x31;
static const uint WaveMatrixCopy              = 0x32;
static const uint WaveMatrixFill              = 0x33;
static const uint Float8Conversion            = 0x36;

enum WaveMatrixOpDataFormat
{
    WaveMatrixDataFormat_I4   = 0x0,
    WaveMatrixDataFormat_U4   = 0x1,
    WaveMatrixDataFormat_I8   = 0x2,
    WaveMatrixDataFormat_U8   = 0x3,
    WaveMatrixDataFormat_F16  = 0x4,
    WaveMatrixDataFormat_BF16 = 0x5,
    WaveMatrixDataFormat_F32  = 0x6,
    WaveMatrixDataFormat_I32  = 0x7,
    WaveMatrixDataFormat_U32  = 0x8,
    WaveMatrixDataFormat_BF8  = 0x9,
    WaveMatrixDataFormat_FP8  = 0xa,
};

enum WaveMatrixOpMatrixType
{
    WaveMatrixType_A            = 0x0,
    WaveMatrixType_B            = 0x1,
    WaveMatrixType_Accumulator  = 0x2,
};

enum WaveMatrixOpMatrixShape
{
    WaveMatrixShape_16X16 = 0x0,
    WaveMatrixShape_32X16 = 0x1,
    WaveMatrixShape_16X32 = 0x2,
    WaveMatrixShape_64X16 = 0x3,
};

enum WaveMatrixOpcode
{
    WMMA_BF16_16X16X16_BF16     = 0x0,
    WMMA_F16_16X16X16_F16       = 0x1,
    WMMA_F32_16X16X16_BF16      = 0x2,
    WMMA_F32_16X16X16_BF8_BF8   = 0x3,
    WMMA_F32_16X16X16_BF8_FP8   = 0x4,
    WMMA_F32_16X16X16_F16       = 0x5,
    WMMA_F32_16X16X16_FP8_BF8   = 0x6,
    WMMA_F32_16X16X16_FP8_FP8   = 0x7,
    WMMA_I32_16X16X16_I4        = 0x8,
    WMMA_I32_16X16X16_U4        = 0x9,
    WMMA_I32_16X16X16_IU4       = 0xa,
    WMMA_I32_16X16X16_UI4       = 0xb,
    WMMA_I32_16X16X16_I8        = 0xc,
    WMMA_I32_16X16X16_U8        = 0xd,
    WMMA_I32_16X16X16_IU8       = 0xe,
    WMMA_I32_16X16X16_UI8       = 0xf,
    WMMA_I32_16X16X32_I4        = 0x10,
    WMMA_I32_16X16X32_U4        = 0x11,
    WMMA_I32_16X16X32_IU4       = 0x12,
    WMMA_I32_16X16X32_UI4       = 0x13,
    SWMMA_BF16_16X16X32_BF16    = 0x14,
    SWMMA_F16_16X16X32_F16      = 0x15,
    SWMMA_F32_16X16X32_BF16     = 0x16,
    SWMMA_F32_16X16X32_BF8_BF8  = 0x17,
    SWMMA_F32_16X16X32_BF8_FP8  = 0x18,
    SWMMA_F32_16X16X32_F16      = 0x19,
    SWMMA_F32_16X16X32_FP8_BF8  = 0x1a,
    SWMMA_F32_16X16X32_FP8_FP8  = 0x1b,
    SWMMA_I32_16X16X32_I4       = 0x1c,
    SWMMA_I32_16X16X32_U4       = 0x1d,
    SWMMA_I32_16X16X32_IU4      = 0x1e,
    SWMMA_I32_16X16X32_UI4      = 0x1f,
    SWMMA_I32_16X16X32_I8       = 0x20,
    SWMMA_I32_16X16X32_U8       = 0x21,
    SWMMA_I32_16X16X32_IU8      = 0x22,
    SWMMA_I32_16X16X32_UI8      = 0x23,
    SWMMA_I32_16X16X64_I4       = 0x24,
    SWMMA_I32_16X16X64_U4       = 0x25,
    SWMMA_I32_16X16X64_IU4      = 0x26,
    SWMMA_I32_16X16X64_UI4      = 0x27,
};

enum WaveMatrixRegType
{
    WaveMatrixRegType_RetVal_Reg          = 0x0,
    WaveMatrixRegType_A_TempReg           = 0x1,
    WaveMatrixRegType_B_TempReg           = 0x2,
    WaveMatrixRegType_Accumulator_TempReg = 0x3,
};

enum MatrixElementWiseOp
{
    MatrixElementWiseOp_Add   = 0x1,
    MatrixElementWiseOp_Sub   = 0x2,
    MatrixElementWiseOp_Mul   = 0x3,
    MatrixElementWiseOp_Div   = 0x4,
    MatrixElementWiseOp_Times = 0x5,
};

enum SparsityIndexMem
{
    SparsityIndexMem_UavBuffer    = 0x0,
    SparsityIndexMem_GroupShared  = 0x1,
    SparsityIndexMem_GlobalBuffer = 0x2,
};

static const uint WaveMatrixOpcode_OpsShift  = 0;
static const uint WaveMatrixOpcode_OpsMask   = 0x7f;
static const uint WaveMatrixOpcode_FlagShift = 15;
static const uint WaveMatrixOpcode_FlagMask  = 0x1;

static const uint WaveMatrixInOut_ChannelShift        = 0;
static const uint WaveMatrixInOut_ChannelMask         = 0xf;
static const uint WaveMatrixInOut_SecondRegFlagShift  = 4;
static const uint WaveMatrixInOut_SecondRegFlagMask   = 0xf;
static const uint WaveMatrixInOut_MatRegTypeFlagShift = 8;
static const uint WaveMatrixInOut_MatRegTypeFlagMask  = 0xff;

static const uint WaveMatrixModifier_DataFormatFlagShift = 0;
static const uint WaveMatrixModifier_DataFormatFlagMask  = 0xf;
static const uint WaveMatrixModifier_MatrixTypeFlagShift = 4;
static const uint WaveMatrixModifier_MatrixTypeFlagMask  = 0x7;
static const uint WaveMatrixModifier_LayoutFlagShift     = 7;
static const uint WaveMatrixModifier_LayoutFlagMask      = 0x1;
static const uint WaveMatrixModifier_ShapeShift          = 8;
static const uint WaveMatrixModifier_ShapeMask           = 0x7;
static const uint WaveMatrixModifier_MatrixTileShift     = 11;
static const uint WaveMatrixModifier_MatrixTileMask      = 0x1;
static const uint WaveMatrixModifier_IndexMemTypeShift   = 14;
static const uint WaveMatrixModifier_IndexMemTypeMask    = 0x3;

uint Code(uint opcode, uint opcodePhase, uint immediateData)
{
    return (MagicCode << MagicCodeShift) |
           ((immediateData & DataMask) << DataShift) |
           ((opcodePhase & OpcodePhaseMask) << OpcodePhaseShift) |
           ((opcode & OpcodeMask) << OpcodeShift);
}

uint AGSMagic(uint code, uint arg0, uint arg1)
{
	uint ret;
	MAGIC.InterlockedCompareExchange(code, arg0, arg1, ret);
	return ret;
}

uint AGSMagic(uint opcode, uint phase, uint imm, uint arg0, uint arg1)
{
	return AGSMagic(Code(opcode, phase, imm), arg0, arg1);
}

uint MatrixIO(uint channel, uint reg, uint type)
{
	return (channel << WaveMatrixInOut_ChannelShift) |
		(reg << WaveMatrixInOut_SecondRegFlagShift) |
		(type << WaveMatrixInOut_MatRegTypeFlagShift);
}

void WMMA_MatMulAcc(uint type, uint4 A, uint4 B, inout uint4 C0, inout uint4 C1)
{
	// A matrix
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(0, 0, WaveMatrixRegType_A_TempReg), A.x, A.y);
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(1, 0, WaveMatrixRegType_A_TempReg), A.z, A.w);
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(0, 1, WaveMatrixRegType_A_TempReg), 0, 0);
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(1, 1, WaveMatrixRegType_A_TempReg), 0, 0);

	// B matrix
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(0, 0, WaveMatrixRegType_B_TempReg), B.x, B.y);
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(1, 0, WaveMatrixRegType_B_TempReg), B.z, B.w);
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(0, 1, WaveMatrixRegType_B_TempReg), 0, 0);
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(1, 1, WaveMatrixRegType_B_TempReg), 0, 0);

	// C matrix
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(0, 0, WaveMatrixRegType_Accumulator_TempReg), C0.x, C0.y);
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(1, 0, WaveMatrixRegType_Accumulator_TempReg), C0.z, C0.w);
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(0, 1, WaveMatrixRegType_Accumulator_TempReg), C1.x, C1.y);
	AGSMagic(WaveMatrixMulAcc, 0, MatrixIO(1, 1, WaveMatrixRegType_Accumulator_TempReg), C1.z, C1.w);

	// Configure type
	AGSMagic(WaveMatrixMulAcc, 1, type << int(WaveMatrixOpcode_OpsShift), 0, 0);

	// Read output
	C0.x = AGSMagic(WaveMatrixMulAcc, 2, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	C0.y = AGSMagic(WaveMatrixMulAcc, 2, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	C0.z = AGSMagic(WaveMatrixMulAcc, 2, MatrixIO(2, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	C0.w = AGSMagic(WaveMatrixMulAcc, 2, MatrixIO(3, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	C1.x = AGSMagic(WaveMatrixMulAcc, 2, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	C1.y = AGSMagic(WaveMatrixMulAcc, 2, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	C1.z = AGSMagic(WaveMatrixMulAcc, 2, MatrixIO(2, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	C1.w = AGSMagic(WaveMatrixMulAcc, 2, MatrixIO(3, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
}

uint LSCode(uint fmt, uint type, uint shape, uint layout)
{
	return (fmt << WaveMatrixModifier_DataFormatFlagShift) |
		(type << WaveMatrixModifier_MatrixTypeFlagShift) |
		(shape << WaveMatrixModifier_ShapeShift) |
		(layout << WaveMatrixModifier_LayoutFlagShift);
}

void WMMA_Load(uint fmt, uint type, uint shape, bool order, ByteAddressBuffer BAB, uint offset, uint stride, out uint4 ret0, out uint4 ret1)
{
	uint doorbell = AGSMagic(WaveMatrixUavLoad, 0, 0, offset, stride);
	uint hook = BAB.Load(doorbell);
	AGSMagic(WaveMatrixUavLoad, 1, LSCode(fmt, type, shape, order), hook, 0);
	ret0.x = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.y = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.z = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(2, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.w = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(3, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.x = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.y = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.z = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(2, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.w = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(3, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
}

void WMMA_Load(uint fmt, uint type, uint shape, bool order, RWByteAddressBuffer BAB, uint offset, uint stride, out uint4 ret0, out uint4 ret1)
{
	uint doorbell = AGSMagic(WaveMatrixUavLoad, 0, 0, offset, stride);
	uint hook = BAB.Load(doorbell);
	AGSMagic(WaveMatrixUavLoad, 1, LSCode(fmt, type, shape, order), hook, 0);
	ret0.x = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.y = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.z = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(2, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.w = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(3, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.x = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.y = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.z = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(2, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.w = AGSMagic(WaveMatrixUavLoad, 2, MatrixIO(3, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
}

void WMMA_Store(uint fmt, uint type, uint shape, bool order, RWByteAddressBuffer BAB, uint offset, uint stride, uint4 V0, uint4 V1)
{
	uint doorbell = AGSMagic(WaveMatrixUavStore, 0, 0, offset, stride);
	uint hook = BAB.Load(doorbell);
	AGSMagic(WaveMatrixUavStore, 1, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg), V0.x, V0.y);
	AGSMagic(WaveMatrixUavStore, 1, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), V0.z, V0.w);
	AGSMagic(WaveMatrixUavStore, 1, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), V1.x, V1.y);
	AGSMagic(WaveMatrixUavStore, 1, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), V1.z, V1.w);
	AGSMagic(WaveMatrixUavStore, 2, LSCode(fmt, type, shape, order), hook, 0);
}

groupshared uint LDS[512];

void WMMA_LoadLDS(uint fmt, uint type, uint shape, bool order, uint offset, uint stride, out uint4 ret0, out uint4 ret1)
{
	uint hook;
	uint doorbell = AGSMagic(WaveMatrixLdsLoad, 0, 0, offset, stride);
	InterlockedAdd(LDS[doorbell], 0, hook);
	AGSMagic(WaveMatrixLdsLoad, 1, LSCode(fmt, type, shape, order), hook, 0);
	ret0.x = AGSMagic(WaveMatrixLdsLoad, 2, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.y = AGSMagic(WaveMatrixLdsLoad, 2, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.z = AGSMagic(WaveMatrixLdsLoad, 2, MatrixIO(2, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.w = AGSMagic(WaveMatrixLdsLoad, 2, MatrixIO(3, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.x = AGSMagic(WaveMatrixLdsLoad, 2, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.y = AGSMagic(WaveMatrixLdsLoad, 2, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.z = AGSMagic(WaveMatrixLdsLoad, 2, MatrixIO(2, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.w = AGSMagic(WaveMatrixLdsLoad, 2, MatrixIO(3, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
}

void WMMA_StoreLDS(uint fmt, uint type, uint shape, bool order, uint offset, uint stride, uint4 V0, uint4 V1)
{
	uint doorbell = AGSMagic(WaveMatrixLdsStore, 0, 0, offset, stride);
	uint hook;
	InterlockedAdd(LDS[doorbell], 0, hook);
	AGSMagic(WaveMatrixLdsStore, 1, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg), V0.x, V0.y);
	AGSMagic(WaveMatrixLdsStore, 1, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), V0.z, V0.w);
	AGSMagic(WaveMatrixLdsStore, 1, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), V1.x, V1.y);
	AGSMagic(WaveMatrixLdsStore, 1, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), V1.z, V1.w);
	AGSMagic(WaveMatrixLdsStore, 2, LSCode(fmt, type, shape, order), hook, 0);
}

void WMMA_Copy(
	uint ifmt, uint itype, uint ishape,
	uint ofmt, uint otype, uint oshape,
	uint4 I0, uint4 I1, out uint4 ret0, out uint4 ret1)
{
	AGSMagic(WaveMatrixCopy, 0, 0, 0, 0);
	AGSMagic(WaveMatrixCopy, 0, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg /* Does this matter? */), I0.x, I0.y);
	AGSMagic(WaveMatrixCopy, 0, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), I0.z, I0.w);
	AGSMagic(WaveMatrixCopy, 0, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), I1.x, I1.y);
	AGSMagic(WaveMatrixCopy, 0, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), I1.z, I1.w);
	AGSMagic(WaveMatrixCopy, 1,
		LSCode(ifmt, itype, ishape, false /* this doesn't seem to matter? */),
		LSCode(ofmt, otype, oshape, false /* this doesn't seem to matter? */), 0);
	ret0.x = AGSMagic(WaveMatrixCopy, 2, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg /* Does this matter? */), 0, 0);
	ret0.y = AGSMagic(WaveMatrixCopy, 2, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.z = AGSMagic(WaveMatrixCopy, 2, MatrixIO(2, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.w = AGSMagic(WaveMatrixCopy, 2, MatrixIO(3, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.x = AGSMagic(WaveMatrixCopy, 2, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.y = AGSMagic(WaveMatrixCopy, 2, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.z = AGSMagic(WaveMatrixCopy, 2, MatrixIO(2, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.w = AGSMagic(WaveMatrixCopy, 2, MatrixIO(3, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
}

uint WMMA_MatrixLength(uint fmt, uint type, uint shape, bool layout)
{
	return AGSMagic(WaveMatrixLength, 0, LSCode(fmt, type, shape, layout), 0, 0);
}

uint WMMA_MatrixElementExtract(uint fmt, uint type, uint shape, bool layout, uint4 I0, uint4 I1, uint elem)
{
	AGSMagic(WaveMatrixElementExtract, 0, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg), I0.x, I0.y);
	AGSMagic(WaveMatrixElementExtract, 0, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), I0.z, I0.w);
	AGSMagic(WaveMatrixElementExtract, 0, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), I1.x, I1.y);
	AGSMagic(WaveMatrixElementExtract, 0, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), I1.z, I1.w);
	return AGSMagic(WaveMatrixElementExtract, 1, LSCode(fmt, type, shape, layout), elem, 0); 
}

void WMMA_MatrixElementFill(uint fmt, uint type, uint shape, bool layout, uint4 I0, uint4 I1, uint index, uint data, out uint4 ret0, out uint4 ret1)
{
	AGSMagic(WaveMatrixElementFill, 0, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg /* Does this matter? */), I0.x, I0.y);
	AGSMagic(WaveMatrixElementFill, 0, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), I0.z, I0.w);
	AGSMagic(WaveMatrixElementFill, 0, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), I1.x, I1.y);
	AGSMagic(WaveMatrixElementFill, 0, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), I1.z, I1.w);
	AGSMagic(WaveMatrixElementFill, 1, LSCode(fmt, type, shape, layout), index, data);
	ret0.x = AGSMagic(WaveMatrixElementFill, 2, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg /* Does this matter? */), 0, 0);
	ret0.y = AGSMagic(WaveMatrixElementFill, 2, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.z = AGSMagic(WaveMatrixElementFill, 2, MatrixIO(2, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.w = AGSMagic(WaveMatrixElementFill, 2, MatrixIO(3, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.x = AGSMagic(WaveMatrixElementFill, 2, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.y = AGSMagic(WaveMatrixElementFill, 2, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.z = AGSMagic(WaveMatrixElementFill, 2, MatrixIO(2, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.w = AGSMagic(WaveMatrixElementFill, 2, MatrixIO(3, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
}

void WMMA_MatrixFill(uint fmt, uint type, uint shape, bool layout, uint value, out uint4 ret0, out uint4 ret1)
{
	AGSMagic(WaveMatrixFill, 0, LSCode(fmt, type, shape, layout), value, 0);
	ret0.x = AGSMagic(WaveMatrixFill, 1, MatrixIO(0, 0, WaveMatrixRegType_RetVal_Reg /* Does this matter? */), 0, 0);
	ret0.y = AGSMagic(WaveMatrixFill, 1, MatrixIO(1, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.z = AGSMagic(WaveMatrixFill, 1, MatrixIO(2, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret0.w = AGSMagic(WaveMatrixFill, 1, MatrixIO(3, 0, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.x = AGSMagic(WaveMatrixFill, 1, MatrixIO(0, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.y = AGSMagic(WaveMatrixFill, 1, MatrixIO(1, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.z = AGSMagic(WaveMatrixFill, 1, MatrixIO(2, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
	ret1.w = AGSMagic(WaveMatrixFill, 1, MatrixIO(3, 1, WaveMatrixRegType_RetVal_Reg), 0, 0);
}

[numthreads(32, 1, 1)]
void main(uint thr : SV_GroupIndex, uint gid : SV_GroupID)
{
	WMMA_StoreLDS(WaveMatrixDataFormat_FP8, WaveMatrixType_Accumulator, WaveMatrixShape_16X16, true, 0, 16, Inputs.Load4(16 * thr), 0);
	GroupMemoryBarrierWithGroupSync();

	uint4 dummy;
	uint4 A, B;
	uint4 C0, C1; 

	WMMA_Load(WaveMatrixDataFormat_F16, WaveMatrixType_A, WaveMatrixShape_16X16, false, Inputs, 0, 32, A, dummy);
	WMMA_LoadLDS(WaveMatrixDataFormat_F16, WaveMatrixType_B, WaveMatrixShape_16X16, true, 0, 32, B, dummy);
	WMMA_Load(WaveMatrixDataFormat_F32, WaveMatrixType_Accumulator, WaveMatrixShape_16X16, false, Inputs, 256, 0, C0, C1);

	WMMA_MatMulAcc(WMMA_F32_16X16X16_F16, A, B, C0, C1);

	//WMMA_MatrixElementFill(WaveMatrixDataFormat_F32, WaveMatrixType_Accumulator, WaveMatrixShape_16X16, false,
	//	C0, C1, 7, 300, C0, C1);
	WMMA_MatrixFill(WaveMatrixDataFormat_FP8, WaveMatrixType_Accumulator, WaveMatrixShape_16X16, false, 0xabcd, C0, C1);

#if 0
	for (int i = 0; i < 8; i++)
	{
		Outputs.Store(4 * i + 32 * thr,
			WMMA_MatrixElementExtract(
			WaveMatrixDataFormat_F32, WaveMatrixType_Accumulator, WaveMatrixShape_16X16, false, C0, C1, i));
	}

	// Quantize output
	WMMA_Copy(
		WaveMatrixDataFormat_F32, WaveMatrixType_Accumulator, WaveMatrixShape_16X16,
		WaveMatrixDataFormat_FP8, WaveMatrixType_Accumulator, WaveMatrixShape_16X16,
		C0, C1, C0, C1);
#endif

	Outputs.Store(0, WMMA_MatrixLength(WaveMatrixDataFormat_F32, WaveMatrixType_Accumulator, WaveMatrixShape_16X16, false));

	WMMA_Store(WaveMatrixDataFormat_FP8, WaveMatrixType_Accumulator, WaveMatrixShape_16X16, true, Outputs, 0, 16, C0, C1);
}
