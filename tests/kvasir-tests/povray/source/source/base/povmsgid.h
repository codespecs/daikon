/****************************************************************************
 *                  povmsgid.h
 *
 * This module contains all defines, typedefs, and prototypes for the POVMS.
 *
 * from Persistence of Vision(tm) Ray Tracer version 3.6.
 * Copyright 1991-2003 Persistence of Vision Team
 * Copyright 2003-2004 Persistence of Vision Raytracer Pty. Ltd.
 *---------------------------------------------------------------------------
 * NOTICE: This source code file is provided so that users may experiment
 * with enhancements to POV-Ray and to port the software to platforms other
 * than those supported by the POV-Ray developers. There are strict rules
 * regarding how you are permitted to use this file. These rules are contained
 * in the distribution and derivative versions licenses which should have been
 * provided with this file.
 *
 * These licences may be found online, linked from the end-user license
 * agreement that is located at http://www.povray.org/povlegal.html
 *---------------------------------------------------------------------------
 * This program is based on the popular DKB raytracer version 2.12.
 * DKBTrace was originally written by David K. Buck.
 * DKBTrace Ver 2.0-2.12 were written by David K. Buck & Aaron A. Collins.
 *---------------------------------------------------------------------------
 * $File: //depot/povray/3.6-release/source/base/povmsgid.h $
 * $Revision: #2 $
 * $Change: 2939 $
 * $DateTime: 2004/07/04 13:43:26 $
 * $Author: root $
 * $Log$
 *****************************************************************************/


#ifndef POVMSGID_H
#define POVMSGID_H

/*****************************************************************************
* NOTE: If you do not understand the code below, stay away! If your compiler
* warns you about the code below, your compiler warning configuration is
* incorrect. Turn that warning off! Do not, ever, mess with the code below to
* just please your compiler. Do not, ever, complain to the POV-Team about the
* code below! Just stay away from this code, please!!!
* RATIONALE: If you do understand the code below and are concerned about
* portability, please direct your attention to the file povms.cpp. There are
* several methods there that do determine the byteorder at runtime. Please
* refer to POVMSStream_Init, POVMSStream_ReadType and POVMSStream_WriteType
* in that file for details. If you have a failure of those functions to
* report, please include as many details about your platform and compiler
* as possible!
******************************************************************************/

/* POV-Ray Object Classes */
enum
{
	kPOVObjectClass_Rect			 = 'Rect',
	kPOVObjectClass_Path			 = 'Path',
	kPOVObjectClass_File			 = 'File',
	kPOVObjectClass_FileLoc			 = 'FPos',
	kPOVObjectClass_Command			 = 'Comm',
	kPOVObjectClass_OIStat			 = 'OISt',
	kPOVObjectClass_ROptions		 = 'ROpt',
	kPOVObjectClass_RTime			 = 'RTim',
	kPOVObjectClass_FStats			 = 'FSta',
	kPOVObjectClass_RStats			 = 'RSta',
	kPOVObjectClass_PStats			 = 'PSta',
	kPOVObjectClass_Progress		 = 'Prog'
};

/* POV-Ray Message Classes */
enum
{
	kPOVMsgClass_RenderControl		 = 'Ctrl',
	kPOVMsgClass_RenderOutput		 = 'Outp',
	kPOVMsgClass_IniOptions			 = 'IniO',
	kPOVMsgClass_Miscellaneous		 = 'Misc'
};

/* POV-Ray Message Identifiers */
enum
{
	kPOVMsgIdent_InitInfo			 = 'InIn',
	kPOVMsgIdent_RenderOptions		 = 'ROpt',
	kPOVMsgIdent_RenderAll			 = 'RAll',
	kPOVMsgIdent_RenderArea			 = 'RAre',
	kPOVMsgIdent_RenderPause		 = 'RPau',
	kPOVMsgIdent_RenderStop			 = 'RSto',
	kPOVMsgIdent_RenderStarted		 = 'RRun',
	kPOVMsgIdent_RenderDone			 = 'REnd',
	kPOVMsgIdent_FrameStatistics	 = 'FSta',
	kPOVMsgIdent_ParseStatistics	 = 'PSta',
	kPOVMsgIdent_RenderStatistics	 = 'RSta',
	kPOVMsgIdent_Progress			 = 'Prog',
	kPOVMsgIdent_Warning			 = 'Warn',
	kPOVMsgIdent_Error				 = 'ErrW',
	kPOVMsgIdent_FatalError			 = 'ErrF',
	kPOVMsgIdent_Debug				 = 'Dbug'
};

/* POV-Ray Message Attributes */
enum
{
	kPOVAttrib_Height				 = 'Heig',
	kPOVAttrib_Width				 = 'Widt',
	kPOVAttrib_TestAbort			 = 'TstA',
	kPOVAttrib_TestAbortCount		 = 'TsAC',
	kPOVAttrib_ContinueTrace		 = 'ConT',
	kPOVAttrib_CreateIni			 = 'CIni',
	kPOVAttrib_Clock				 = 'Clck',
	kPOVAttrib_InitialFrame			 = 'IFrm',
	kPOVAttrib_FinalFrame			 = 'FFrm',
	kPOVAttrib_InitialClock			 = 'IClk',
	kPOVAttrib_FinalClock			 = 'FClk',
	kPOVAttrib_SubsetStartFrame		 = 'SStF',
	kPOVAttrib_SubsetEndFrame		 = 'SEnF',
	kPOVAttrib_CyclicAnimation		 = 'CylA',
	kPOVAttrib_FieldRender			 = 'FldR',
	kPOVAttrib_OddField				 = 'OddF',
	kPOVAttrib_PauseWhenDone		 = 'PWDo',
	kPOVAttrib_Verbose				 = 'Verb',
	kPOVAttrib_DrawVistas			 = 'DrVi',
	kPOVAttrib_Display				 = 'Disp',
	kPOVAttrib_VideoMode			 = 'VMod',
	kPOVAttrib_Palette				 = 'Palt',
	kPOVAttrib_DisplayGamma			 = 'DGam',
	kPOVAttrib_PreviewStartSize		 = 'PStS',
	kPOVAttrib_PreviewEndSize		 = 'PEnS',
	kPOVAttrib_OutputToFile			 = 'OToF',
	kPOVAttrib_OutputFileType		 = 'OFTy',
	kPOVAttrib_OutputAlpha			 = 'OAlp',
	kPOVAttrib_BitsPerColor			 = 'BPCo',
	kPOVAttrib_OutputFile			 = 'OFNa',
	kPOVAttrib_OutputPath			 = 'OPat',
	kPOVAttrib_Compression			 = 'OFCo',
	kPOVAttrib_CreateHistogram		 = 'CHis',
	kPOVAttrib_HistogramFileType	 = 'HFTy',
	kPOVAttrib_HistogramFile		 = 'HFNa',
	kPOVAttrib_HistogramGridSizeX	 = 'HGSX',
	kPOVAttrib_HistogramGridSizeY	 = 'HGSY',
	kPOVAttrib_PreSceneCommand		 = 'PrSC',
	kPOVAttrib_PreFrameCommand		 = 'PrFC',
	kPOVAttrib_PostSceneCommand		 = 'PoSc',
	kPOVAttrib_PostFrameCommand		 = 'PoFC',
	kPOVAttrib_UserAbortCommand		 = 'UAbC',
	kPOVAttrib_FatalErrorCommand	 = 'FErC',
	kPOVAttrib_InputFile			 = 'IFNa',
	kPOVAttrib_LibraryPath			 = 'LibP',
	kPOVAttrib_Version				 = 'Vers',
	kPOVAttrib_DebugConsole			 = 'DCon',
	kPOVAttrib_FatalConsole			 = 'FCon',
	kPOVAttrib_RenderConsole		 = 'RCon',
	kPOVAttrib_StatisticsConsole	 = 'SCon',
	kPOVAttrib_WarningConsole		 = 'WCon',
	kPOVAttrib_AllConsole			 = 'ACon',
	kPOVAttrib_Declare				 = 'Decl',
	kPOVAttrib_DebugFile			 = 'DFNa',
	kPOVAttrib_FatalFile			 = 'FFNa',
	kPOVAttrib_RenderFile			 = 'RFNa',
	kPOVAttrib_StatisticsFile		 = 'SFNa',
	kPOVAttrib_WarningFile			 = 'WFNa',
	kPOVAttrib_WarningLevel			 = 'WLev',
	kPOVAttrib_AllFile				 = 'AFNa',
	kPOVAttrib_Quality				 = 'Qual',
	kPOVAttrib_Bounding				 = 'Boun',
	kPOVAttrib_BoundingThreshold	 = 'BdTh',
	kPOVAttrib_LightBuffer			 = 'LBuf',
	kPOVAttrib_VistaBuffer			 = 'VBuf',
	kPOVAttrib_Radiosity			 = 'Radi',
	kPOVAttrib_RemoveBounds			 = 'RmBd',
	kPOVAttrib_SplitUnions			 = 'SplU',
	kPOVAttrib_Antialias			 = 'Anti',
	kPOVAttrib_SamplingMethod		 = 'AASM',
	kPOVAttrib_AntialiasThreshold	 = 'AATh',
	kPOVAttrib_AntialiasDepth		 = 'AADe',
	kPOVAttrib_Jitter				 = 'AAJi',
	kPOVAttrib_JitterAmount			 = 'AAJA',
	kPOVAttrib_Left					 = 'Left',
	kPOVAttrib_Top					 = 'Top ',
	kPOVAttrib_Right				 = 'Righ',
	kPOVAttrib_Bottom				 = 'Bott',
	kPOVAttrib_CoreVersion			 = 'Core',
	kPOVAttrib_PlatformName			 = 'Plat',
	kPOVAttrib_Official				 = 'Offi',
	kPOVAttrib_PrimaryDevs			 = 'Prim',
	kPOVAttrib_ContributingDevs		 = 'Cont',
	kPOVAttrib_ImageLibVersions		 = 'ILVe',
	kPOVAttrib_EnglishText			 = 'ETxt',
	kPOVAttrib_CurrentToken			 = 'CurT',
	kPOVAttrib_CurrentLine			 = 'CurL',
	kPOVAttrib_LineCount			 = 'LCnt',
	kPOVAttrib_AbsoluteCurrentLine	 = 'AbsL',
	kPOVAttrib_FiniteObjects		 = 'FiOb',
	kPOVAttrib_InfiniteObjects		 = 'InOb',
	kPOVAttrib_LightSources			 = 'LiSo',
	kPOVAttrib_CurrentFrame			 = 'CurF',
	kPOVAttrib_FrameCount			 = 'FCnt',
	kPOVAttrib_AbsoluteCurFrame		 = 'AbsF',
	kPOVAttrib_FirstClock			 = 'FirC',
	kPOVAttrib_CurrentClock			 = 'CurC',
	kPOVAttrib_LastClock			 = 'LasC',
	kPOVAttrib_TotalPhotonCount		 = 'TPCn',
	kPOVAttrib_CurrentPhotonCount	 = 'CPCn',
	kPOVAttrib_ObjectPhotonCount	 = 'OPCn',
	kPOVAttrib_MediaPhotonCount		 = 'MPCn',
	kPOVAttrib_PhotonXSamples		 = 'PXSa',
	kPOVAttrib_PhotonYSamples		 = 'PYSa',
	kPOVAttrib_ProgressStatus		 = 'ProS',
	kPOVAttrib_ParseTime			 = 'ParT',
	kPOVAttrib_PhotonTime			 = 'PhoT',
	kPOVAttrib_TraceTime			 = 'TraT',
	kPOVAttrib_TotalTime			 = 'TotT',
	kPOVAttrib_FileName				 = 'File',
	kPOVAttrib_Pixels				 = 'Pixe',
	kPOVAttrib_PixelSamples			 = 'PixS',
	kPOVAttrib_SuperSampleCount		 = 'SSCn',
	kPOVAttrib_MosaicPreviewSize	 = 'MPSi',
	kPOVAttrib_Rays					 = 'Rays',
	kPOVAttrib_RaysSaved			 = 'RSav',
	kPOVAttrib_TraceLevel			 = 'TLev',
	kPOVAttrib_MaxTraceLevel		 = 'MaxL',
	kPOVAttrib_ObjectIStats			 = 'OISt',
	kPOVAttrib_ShadowTest			 = 'ShdT',
	kPOVAttrib_ShadowTestSuc		 = 'ShdS',
	kPOVAttrib_PolynomTest			 = 'PnmT',
	kPOVAttrib_RootsEliminated		 = 'REli',
	kPOVAttrib_CallsToNoise			 = 'CTNo',
	kPOVAttrib_CallsToDNoise		 = 'CTDN',
	kPOVAttrib_MediaSamples			 = 'MeSa',
	kPOVAttrib_MediaIntervals		 = 'MeIn',
	kPOVAttrib_ReflectedRays		 = 'RflR',
	kPOVAttrib_InnerReflectedRays	 = 'IReR',
	kPOVAttrib_RefractedRays		 = 'RfrT',
	kPOVAttrib_TransmittedRays		 = 'TraR',
	kPOVAttrib_IStackOverflow		 = 'IStO',
	kPOVAttrib_RadGatherCount		 = 'RGCt',
	kPOVAttrib_RadReuseCount		 = 'RRCt',
	kPOVAttrib_MinAlloc				 = 'MinA',
	kPOVAttrib_MaxAlloc				 = 'MaxA',
	kPOVAttrib_CallsToAlloc			 = 'CTAl',
	kPOVAttrib_CallsToFree			 = 'CTFr',
	kPOVAttrib_PeakMemoryUsage		 = 'PMUs',
	kPOVAttrib_PhotonsShot			 = 'PSho',
	kPOVAttrib_PhotonsStored		 = 'PSto',
	kPOVAttrib_GlobalPhotonsStored	 = 'GPSt',
	kPOVAttrib_MediaPhotonsStored	 = 'MPSt',
	kPOVAttrib_PhotonsPriQInsert	 = 'PPQI',
	kPOVAttrib_PhotonsPriQRemove	 = 'PPQR',
	kPOVAttrib_GatherPerformedCnt	 = 'GPCn',
	kPOVAttrib_GatherExpandedCnt	 = 'GECn',
	kPOVAttrib_IsoFindRoot			 = 'IFRo',
	kPOVAttrib_FunctionVMCalls		 = 'FVMC',
	kPOVAttrib_FunctionVMInstrEst	 = 'FVMI',
	kPOVAttrib_State				 = 'Stat',
	kPOVAttrib_Warning				 = 'Warn',
	kPOVAttrib_Line					 = 'Line',
	kPOVAttrib_Column				 = 'Colu',
	kPOVAttrib_FilePosition			 = 'FPos',
	kPOVAttrib_TokenName			 = 'TokN',
	kPOVAttrib_Error				 = 'Erro',
	kPOVAttrib_ObjectName			 = 'ONam',
	kPOVAttrib_ObjectID				 = 'OIde',
	kPOVAttrib_ISectsTests			 = 'ITst',
	kPOVAttrib_ISectsSucceeded		 = 'ISuc',
	kPOVAttrib_CommandString		 = 'ComS',
	kPOVAttrib_ReturnAction			 = 'RAct',
	kPOVAttrib_FrameTime			 = 'FTim',
	kPOVAttrib_AnimationTime		 = 'ATim',
	kPOVAttrib_IncludeHeader		 = 'IncH',
	kPOVAttrib_IncludeIni			 = 'IncI',
	kPOVAttrib_PreviewRefCon		 = 'PRef',
	kPOVAttrib_INIFile				 = 'IFil',
	kPOVAttrib_RenderOptions		 = 'ROpt',
	kPOVAttrib_Identifier			 = 'Iden',
	kPOVAttrib_Value				 = 'Valu',
	kPOVAttrib_StartColumn			 = kPOVAttrib_Left,
	kPOVAttrib_EndColumn			 = kPOVAttrib_Right,
	kPOVAttrib_StartRow				 = kPOVAttrib_Top,
	kPOVAttrib_EndRow				 = kPOVAttrib_Bottom
};

/* Add new stats ONLY at the end!!! */
enum
{
	kPOVList_Stat_BicubicTest = 1,
	kPOVList_Stat_BlobTest,
	kPOVList_Stat_BlobCpTest,
	kPOVList_Stat_BlobBdTest,
	kPOVList_Stat_BoxTest,
	kPOVList_Stat_ConeCylTest,
	kPOVList_Stat_CSGIntersectTest,
	kPOVList_Stat_CSGMergeTest,
	kPOVList_Stat_CSGUnionTest,
	kPOVList_Stat_DiscTest,
	kPOVList_Stat_FractalTest,
	kPOVList_Stat_HFTest,
	kPOVList_Stat_HFBoxTest,
	kPOVList_Stat_HFTriangleTest,
	kPOVList_Stat_HFBlockTest,
	kPOVList_Stat_HFCellTest,
	kPOVList_Stat_IsosurfaceTest,
	kPOVList_Stat_IsosurfaceBdTest,
	kPOVList_Stat_IsosurfaceCacheTest,
	kPOVList_Stat_LatheTest,
	kPOVList_Stat_LatheBdTest,
	kPOVList_Stat_MeshTest,
	kPOVList_Stat_PlaneTest,
	kPOVList_Stat_PolygonTest,
	kPOVList_Stat_PrismTest,
	kPOVList_Stat_PrismBdTest,
	kPOVList_Stat_ParametricTest,
	kPOVList_Stat_ParametricBoxTest,
	kPOVList_Stat_QuardicTest,
	kPOVList_Stat_QuadPolyTest,
	kPOVList_Stat_SphereTest,
	kPOVList_Stat_SphereSweepTest,
	kPOVList_Stat_SuperellipsTest,
	kPOVList_Stat_SORTest,
	kPOVList_Stat_SORBdTest,
	kPOVList_Stat_TorusTest,
	kPOVList_Stat_TorusBdTest,
	kPOVList_Stat_TriangleTest,
	kPOVList_Stat_TTFontTest,
	kPOVList_Stat_BoundObjectTest,
	kPOVList_Stat_ClipObjectTest,
	kPOVList_Stat_BoundingBoxTest,
	kPOVList_Stat_LightBufferTest,
	kPOVList_Stat_VistaBufferTest,
	kPOVList_Stat_RBezierTest,
	kPOVList_Stat_Last
};

/* Add new progress messages ONLY at the end!!! */
enum
{
	kPOVList_Prog_CreatingBoundingSlabs = 1,
	kPOVList_Prog_CreatingVistaBuffer,
	kPOVList_Prog_CreatingLightBuffers,
	kPOVList_Prog_BuildingPhotonMaps,
	kPOVList_Prog_LoadingPhotonMaps,
	kPOVList_Prog_SavingPhotonMaps,
	kPOVList_Prog_SortingPhotons,
	kPOVList_Prog_ReclaimingMemory,
	kPOVList_Prog_WritingINIFile,
	kPOVList_Prog_WritingHistogramFile,
	kPOVList_Prog_PerformingShelloutCommand,
	kPOVList_Prog_ResumingInterruptedTrace,
	kPOVList_Prog_ProcessingFrame,
	kPOVList_Prog_Parsing,
	kPOVList_Prog_Displaying,
	kPOVList_Prog_Rendering,
	kPOVList_Prog_DoneTracing,
	kPOVList_Prog_AbortingRender,
	kPOVList_Prog_UserAbort
};

#endif /* POVMSGID_H */
