//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// DAG Instruction Selector for the PPC target
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

// *** NOTE: This file is #included into the middle of the target
// *** instruction selector class.  These functions are really methods.

// The main instruction selector code.
SDNode *SelectCode(SDNode *N) {
  // Opcodes are emitted as 2 bytes, TARGET_OPCODE handles this.
  #define TARGET_OPCODE(X) X & 255, unsigned(X) >> 8
  static const unsigned char MatcherTable[] = {
/*0*/     OPC_SwitchOpcode /*94 cases */, 40,  TARGET_OPCODE(ISD::MEMBARRIER),// ->44
/*4*/       OPC_RecordNode,   // #0 = 'membarrier' chained node
/*5*/       OPC_MoveChild, 1,
/*7*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*10*/      OPC_CheckType, MVT::i32,
/*12*/      OPC_MoveParent,
/*13*/      OPC_MoveChild, 2,
/*15*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*18*/      OPC_MoveParent,
/*19*/      OPC_MoveChild, 3,
/*21*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*24*/      OPC_MoveParent,
/*25*/      OPC_MoveChild, 4,
/*27*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*30*/      OPC_MoveParent,
/*31*/      OPC_MoveChild, 5,
/*33*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*36*/      OPC_MoveParent,
/*37*/      OPC_EmitMergeInputChains1_0,
/*38*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::SYNC), 0|OPFL_Chain,
                0/*#VTs*/, 0/*#Ops*/, 
            // Src: (membarrier (imm:i32), (imm:i32), (imm:i32), (imm:i32), (imm:i32)) - Complexity = 18
            // Dst: (SYNC)
          /*SwitchOpcode*/ 73|128,4/*585*/,  TARGET_OPCODE(ISD::INTRINSIC_VOID),// ->633
/*48*/      OPC_RecordNode,   // #0 = 'intrinsic_void' chained node
/*49*/      OPC_MoveChild, 1,
/*51*/      OPC_Scope, 17, /*->70*/ // 21 children in Scope
/*53*/        OPC_CheckInteger, 67|128,2/*323*/, 
/*56*/        OPC_MoveParent,
/*57*/        OPC_RecordChild2, // #1 = $dst
/*58*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$dst #2 #3
/*61*/        OPC_EmitMergeInputChains1_0,
/*62*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::DCBA), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_void 323:iPTR, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (DCBA xoaddr:iPTR:$dst)
/*70*/      /*Scope*/ 17, /*->88*/
/*71*/        OPC_CheckInteger, 68|128,2/*324*/, 
/*74*/        OPC_MoveParent,
/*75*/        OPC_RecordChild2, // #1 = $dst
/*76*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$dst #2 #3
/*79*/        OPC_EmitMergeInputChains1_0,
/*80*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::DCBF), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_void 324:iPTR, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (DCBF xoaddr:iPTR:$dst)
/*88*/      /*Scope*/ 17, /*->106*/
/*89*/        OPC_CheckInteger, 69|128,2/*325*/, 
/*92*/        OPC_MoveParent,
/*93*/        OPC_RecordChild2, // #1 = $dst
/*94*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$dst #2 #3
/*97*/        OPC_EmitMergeInputChains1_0,
/*98*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::DCBI), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_void 325:iPTR, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (DCBI xoaddr:iPTR:$dst)
/*106*/     /*Scope*/ 17, /*->124*/
/*107*/       OPC_CheckInteger, 70|128,2/*326*/, 
/*110*/       OPC_MoveParent,
/*111*/       OPC_RecordChild2, // #1 = $dst
/*112*/       OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$dst #2 #3
/*115*/       OPC_EmitMergeInputChains1_0,
/*116*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::DCBST), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_void 326:iPTR, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (DCBST xoaddr:iPTR:$dst)
/*124*/     /*Scope*/ 17, /*->142*/
/*125*/       OPC_CheckInteger, 71|128,2/*327*/, 
/*128*/       OPC_MoveParent,
/*129*/       OPC_RecordChild2, // #1 = $dst
/*130*/       OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$dst #2 #3
/*133*/       OPC_EmitMergeInputChains1_0,
/*134*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::DCBT), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_void 327:iPTR, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (DCBT xoaddr:iPTR:$dst)
/*142*/     /*Scope*/ 17, /*->160*/
/*143*/       OPC_CheckInteger, 72|128,2/*328*/, 
/*146*/       OPC_MoveParent,
/*147*/       OPC_RecordChild2, // #1 = $dst
/*148*/       OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$dst #2 #3
/*151*/       OPC_EmitMergeInputChains1_0,
/*152*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::DCBTST), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_void 328:iPTR, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (DCBTST xoaddr:iPTR:$dst)
/*160*/     /*Scope*/ 17, /*->178*/
/*161*/       OPC_CheckInteger, 73|128,2/*329*/, 
/*164*/       OPC_MoveParent,
/*165*/       OPC_RecordChild2, // #1 = $dst
/*166*/       OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$dst #2 #3
/*169*/       OPC_EmitMergeInputChains1_0,
/*170*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::DCBZ), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_void 329:iPTR, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (DCBZ xoaddr:iPTR:$dst)
/*178*/     /*Scope*/ 17, /*->196*/
/*179*/       OPC_CheckInteger, 74|128,2/*330*/, 
/*182*/       OPC_MoveParent,
/*183*/       OPC_RecordChild2, // #1 = $dst
/*184*/       OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$dst #2 #3
/*187*/       OPC_EmitMergeInputChains1_0,
/*188*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::DCBZL), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_void 330:iPTR, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (DCBZL xoaddr:iPTR:$dst)
/*196*/     /*Scope*/ 19, /*->216*/
/*197*/       OPC_CheckInteger, 63|128,1/*191*/, 
/*200*/       OPC_MoveParent,
/*201*/       OPC_RecordChild2, // #1 = $rS
/*202*/       OPC_RecordChild3, // #2 = $dst
/*203*/       OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*206*/       OPC_EmitMergeInputChains1_0,
/*207*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::STVEBX), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
              // Src: (intrinsic_void 191:iPTR, VRRC:v16i8:$rS, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (STVEBX VRRC:v16i8:$rS, xoaddr:iPTR:$dst)
/*216*/     /*Scope*/ 19, /*->236*/
/*217*/       OPC_CheckInteger, 64|128,1/*192*/, 
/*220*/       OPC_MoveParent,
/*221*/       OPC_RecordChild2, // #1 = $rS
/*222*/       OPC_RecordChild3, // #2 = $dst
/*223*/       OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*226*/       OPC_EmitMergeInputChains1_0,
/*227*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::STVEHX), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
              // Src: (intrinsic_void 192:iPTR, VRRC:v8i16:$rS, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (STVEHX VRRC:v8i16:$rS, xoaddr:iPTR:$dst)
/*236*/     /*Scope*/ 19, /*->256*/
/*237*/       OPC_CheckInteger, 65|128,1/*193*/, 
/*240*/       OPC_MoveParent,
/*241*/       OPC_RecordChild2, // #1 = $rS
/*242*/       OPC_RecordChild3, // #2 = $dst
/*243*/       OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*246*/       OPC_EmitMergeInputChains1_0,
/*247*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::STVEWX), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
              // Src: (intrinsic_void 193:iPTR, VRRC:v4i32:$rS, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (STVEWX VRRC:v4i32:$rS, xoaddr:iPTR:$dst)
/*256*/     /*Scope*/ 19, /*->276*/
/*257*/       OPC_CheckInteger, 66|128,1/*194*/, 
/*260*/       OPC_MoveParent,
/*261*/       OPC_RecordChild2, // #1 = $rS
/*262*/       OPC_RecordChild3, // #2 = $dst
/*263*/       OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*266*/       OPC_EmitMergeInputChains1_0,
/*267*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::STVX), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
              // Src: (intrinsic_void 194:iPTR, VRRC:v4i32:$rS, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (STVX VRRC:v4i32:$rS, xoaddr:iPTR:$dst)
/*276*/     /*Scope*/ 19, /*->296*/
/*277*/       OPC_CheckInteger, 67|128,1/*195*/, 
/*280*/       OPC_MoveParent,
/*281*/       OPC_RecordChild2, // #1 = $rS
/*282*/       OPC_RecordChild3, // #2 = $dst
/*283*/       OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*286*/       OPC_EmitMergeInputChains1_0,
/*287*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::STVXL), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
              // Src: (intrinsic_void 195:iPTR, VRRC:v4i32:$rS, xoaddr:iPTR:$dst) - Complexity = 17
              // Dst: (STVXL VRRC:v4i32:$rS, xoaddr:iPTR:$dst)
/*296*/     /*Scope*/ 33, /*->330*/
/*297*/       OPC_CheckInteger, 48|128,1/*176*/, 
/*300*/       OPC_MoveParent,
/*301*/       OPC_RecordChild2, // #1 = $STRM
/*302*/       OPC_MoveChild, 2,
/*304*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*307*/       OPC_MoveParent,
/*308*/       OPC_EmitMergeInputChains1_0,
/*309*/       OPC_EmitInteger, MVT::i32, 0, 
/*312*/       OPC_EmitConvertToTarget, 1,
/*314*/       OPC_EmitInteger, MVT::i32, 0, 
/*317*/       OPC_EmitInteger, MVT::i32, 0, 
/*320*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::DSS), 0|OPFL_Chain,
                  0/*#VTs*/, 4/*#Ops*/, 2, 3, 4, 5, 
              // Src: (intrinsic_void 176:iPTR, (imm:i32):$STRM) - Complexity = 11
              // Dst: (DSS 0:i32, (imm:i32):$STRM, 0:i32, 0:i32)
/*330*/     /*Scope*/ 61, /*->392*/
/*331*/       OPC_CheckInteger, 50|128,1/*178*/, 
/*334*/       OPC_MoveParent,
/*335*/       OPC_RecordChild2, // #1 = $rA
/*336*/       OPC_Scope, 26, /*->364*/ // 2 children in Scope
/*338*/         OPC_CheckChild2Type, MVT::i32,
/*340*/         OPC_RecordChild3, // #2 = $rB
/*341*/         OPC_RecordChild4, // #3 = $STRM
/*342*/         OPC_MoveChild, 4,
/*344*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*347*/         OPC_MoveParent,
/*348*/         OPC_EmitMergeInputChains1_0,
/*349*/         OPC_EmitInteger, MVT::i32, 0, 
/*352*/         OPC_EmitConvertToTarget, 3,
/*354*/         OPC_MorphNodeTo, TARGET_OPCODE(PPC::DST), 0|OPFL_Chain,
                    0/*#VTs*/, 4/*#Ops*/, 4, 5, 1, 2, 
                // Src: (intrinsic_void 178:iPTR, GPRC:i32:$rA, GPRC:i32:$rB, (imm:i32):$STRM) - Complexity = 11
                // Dst: (DST 0:i32, (imm:i32):$STRM, GPRC:i32:$rA, GPRC:i32:$rB)
/*364*/       /*Scope*/ 26, /*->391*/
/*365*/         OPC_CheckChild2Type, MVT::i64,
/*367*/         OPC_RecordChild3, // #2 = $rB
/*368*/         OPC_RecordChild4, // #3 = $STRM
/*369*/         OPC_MoveChild, 4,
/*371*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*374*/         OPC_MoveParent,
/*375*/         OPC_EmitMergeInputChains1_0,
/*376*/         OPC_EmitInteger, MVT::i32, 0, 
/*379*/         OPC_EmitConvertToTarget, 3,
/*381*/         OPC_MorphNodeTo, TARGET_OPCODE(PPC::DST64), 0|OPFL_Chain,
                    0/*#VTs*/, 4/*#Ops*/, 4, 5, 1, 2, 
                // Src: (intrinsic_void 178:iPTR, G8RC:i64:$rA, GPRC:i32:$rB, (imm:i32):$STRM) - Complexity = 11
                // Dst: (DST64 0:i32, (imm:i32):$STRM, G8RC:i64:$rA, GPRC:i32:$rB)
/*391*/       0, /*End of Scope*/
/*392*/     /*Scope*/ 61, /*->454*/
/*393*/       OPC_CheckInteger, 53|128,1/*181*/, 
/*396*/       OPC_MoveParent,
/*397*/       OPC_RecordChild2, // #1 = $rA
/*398*/       OPC_Scope, 26, /*->426*/ // 2 children in Scope
/*400*/         OPC_CheckChild2Type, MVT::i32,
/*402*/         OPC_RecordChild3, // #2 = $rB
/*403*/         OPC_RecordChild4, // #3 = $STRM
/*404*/         OPC_MoveChild, 4,
/*406*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*409*/         OPC_MoveParent,
/*410*/         OPC_EmitMergeInputChains1_0,
/*411*/         OPC_EmitInteger, MVT::i32, 1, 
/*414*/         OPC_EmitConvertToTarget, 3,
/*416*/         OPC_MorphNodeTo, TARGET_OPCODE(PPC::DSTT), 0|OPFL_Chain,
                    0/*#VTs*/, 4/*#Ops*/, 4, 5, 1, 2, 
                // Src: (intrinsic_void 181:iPTR, GPRC:i32:$rA, GPRC:i32:$rB, (imm:i32):$STRM) - Complexity = 11
                // Dst: (DSTT 1:i32, (imm:i32):$STRM, GPRC:i32:$rA, GPRC:i32:$rB)
/*426*/       /*Scope*/ 26, /*->453*/
/*427*/         OPC_CheckChild2Type, MVT::i64,
/*429*/         OPC_RecordChild3, // #2 = $rB
/*430*/         OPC_RecordChild4, // #3 = $STRM
/*431*/         OPC_MoveChild, 4,
/*433*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*436*/         OPC_MoveParent,
/*437*/         OPC_EmitMergeInputChains1_0,
/*438*/         OPC_EmitInteger, MVT::i32, 1, 
/*441*/         OPC_EmitConvertToTarget, 3,
/*443*/         OPC_MorphNodeTo, TARGET_OPCODE(PPC::DSTT64), 0|OPFL_Chain,
                    0/*#VTs*/, 4/*#Ops*/, 4, 5, 1, 2, 
                // Src: (intrinsic_void 181:iPTR, G8RC:i64:$rA, GPRC:i32:$rB, (imm:i32):$STRM) - Complexity = 11
                // Dst: (DSTT64 1:i32, (imm:i32):$STRM, G8RC:i64:$rA, GPRC:i32:$rB)
/*453*/       0, /*End of Scope*/
/*454*/     /*Scope*/ 61, /*->516*/
/*455*/       OPC_CheckInteger, 51|128,1/*179*/, 
/*458*/       OPC_MoveParent,
/*459*/       OPC_RecordChild2, // #1 = $rA
/*460*/       OPC_Scope, 26, /*->488*/ // 2 children in Scope
/*462*/         OPC_CheckChild2Type, MVT::i32,
/*464*/         OPC_RecordChild3, // #2 = $rB
/*465*/         OPC_RecordChild4, // #3 = $STRM
/*466*/         OPC_MoveChild, 4,
/*468*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*471*/         OPC_MoveParent,
/*472*/         OPC_EmitMergeInputChains1_0,
/*473*/         OPC_EmitInteger, MVT::i32, 0, 
/*476*/         OPC_EmitConvertToTarget, 3,
/*478*/         OPC_MorphNodeTo, TARGET_OPCODE(PPC::DSTST), 0|OPFL_Chain,
                    0/*#VTs*/, 4/*#Ops*/, 4, 5, 1, 2, 
                // Src: (intrinsic_void 179:iPTR, GPRC:i32:$rA, GPRC:i32:$rB, (imm:i32):$STRM) - Complexity = 11
                // Dst: (DSTST 0:i32, (imm:i32):$STRM, GPRC:i32:$rA, GPRC:i32:$rB)
/*488*/       /*Scope*/ 26, /*->515*/
/*489*/         OPC_CheckChild2Type, MVT::i64,
/*491*/         OPC_RecordChild3, // #2 = $rB
/*492*/         OPC_RecordChild4, // #3 = $STRM
/*493*/         OPC_MoveChild, 4,
/*495*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*498*/         OPC_MoveParent,
/*499*/         OPC_EmitMergeInputChains1_0,
/*500*/         OPC_EmitInteger, MVT::i32, 0, 
/*503*/         OPC_EmitConvertToTarget, 3,
/*505*/         OPC_MorphNodeTo, TARGET_OPCODE(PPC::DSTST64), 0|OPFL_Chain,
                    0/*#VTs*/, 4/*#Ops*/, 4, 5, 1, 2, 
                // Src: (intrinsic_void 179:iPTR, G8RC:i64:$rA, GPRC:i32:$rB, (imm:i32):$STRM) - Complexity = 11
                // Dst: (DSTST64 0:i32, (imm:i32):$STRM, G8RC:i64:$rA, GPRC:i32:$rB)
/*515*/       0, /*End of Scope*/
/*516*/     /*Scope*/ 61, /*->578*/
/*517*/       OPC_CheckInteger, 52|128,1/*180*/, 
/*520*/       OPC_MoveParent,
/*521*/       OPC_RecordChild2, // #1 = $rA
/*522*/       OPC_Scope, 26, /*->550*/ // 2 children in Scope
/*524*/         OPC_CheckChild2Type, MVT::i32,
/*526*/         OPC_RecordChild3, // #2 = $rB
/*527*/         OPC_RecordChild4, // #3 = $STRM
/*528*/         OPC_MoveChild, 4,
/*530*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*533*/         OPC_MoveParent,
/*534*/         OPC_EmitMergeInputChains1_0,
/*535*/         OPC_EmitInteger, MVT::i32, 1, 
/*538*/         OPC_EmitConvertToTarget, 3,
/*540*/         OPC_MorphNodeTo, TARGET_OPCODE(PPC::DSTSTT), 0|OPFL_Chain,
                    0/*#VTs*/, 4/*#Ops*/, 4, 5, 1, 2, 
                // Src: (intrinsic_void 180:iPTR, GPRC:i32:$rA, GPRC:i32:$rB, (imm:i32):$STRM) - Complexity = 11
                // Dst: (DSTSTT 1:i32, (imm:i32):$STRM, GPRC:i32:$rA, GPRC:i32:$rB)
/*550*/       /*Scope*/ 26, /*->577*/
/*551*/         OPC_CheckChild2Type, MVT::i64,
/*553*/         OPC_RecordChild3, // #2 = $rB
/*554*/         OPC_RecordChild4, // #3 = $STRM
/*555*/         OPC_MoveChild, 4,
/*557*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*560*/         OPC_MoveParent,
/*561*/         OPC_EmitMergeInputChains1_0,
/*562*/         OPC_EmitInteger, MVT::i32, 1, 
/*565*/         OPC_EmitConvertToTarget, 3,
/*567*/         OPC_MorphNodeTo, TARGET_OPCODE(PPC::DSTSTT64), 0|OPFL_Chain,
                    0/*#VTs*/, 4/*#Ops*/, 4, 5, 1, 2, 
                // Src: (intrinsic_void 180:iPTR, G8RC:i64:$rA, GPRC:i32:$rB, (imm:i32):$STRM) - Complexity = 11
                // Dst: (DSTSTT64 1:i32, (imm:i32):$STRM, G8RC:i64:$rA, GPRC:i32:$rB)
/*577*/       0, /*End of Scope*/
/*578*/     /*Scope*/ 11, /*->590*/
/*579*/       OPC_CheckInteger, 75|128,2/*331*/, 
/*582*/       OPC_MoveParent,
/*583*/       OPC_EmitMergeInputChains1_0,
/*584*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::SYNC), 0|OPFL_Chain,
                  0/*#VTs*/, 0/*#Ops*/, 
              // Src: (intrinsic_void 331:iPTR) - Complexity = 8
              // Dst: (SYNC)
/*590*/     /*Scope*/ 13, /*->604*/
/*591*/       OPC_CheckInteger, 62|128,1/*190*/, 
/*594*/       OPC_MoveParent,
/*595*/       OPC_RecordChild2, // #1 = $vB
/*596*/       OPC_EmitMergeInputChains1_0,
/*597*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::MTVSCR), 0|OPFL_Chain,
                  0/*#VTs*/, 1/*#Ops*/, 1, 
              // Src: (intrinsic_void 190:iPTR, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (MTVSCR VRRC:v4i32:$vB)
/*604*/     /*Scope*/ 27, /*->632*/
/*605*/       OPC_CheckInteger, 49|128,1/*177*/, 
/*608*/       OPC_MoveParent,
/*609*/       OPC_EmitMergeInputChains1_0,
/*610*/       OPC_EmitInteger, MVT::i32, 1, 
/*613*/       OPC_EmitInteger, MVT::i32, 0, 
/*616*/       OPC_EmitInteger, MVT::i32, 0, 
/*619*/       OPC_EmitInteger, MVT::i32, 0, 
/*622*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::DSSALL), 0|OPFL_Chain,
                  0/*#VTs*/, 4/*#Ops*/, 1, 2, 3, 4, 
              // Src: (intrinsic_void 177:iPTR) - Complexity = 8
              // Dst: (DSSALL 1:i32, 0:i32, 0:i32, 0:i32)
/*632*/     0, /*End of Scope*/
          /*SwitchOpcode*/ 32|128,4/*544*/,  TARGET_OPCODE(ISD::ADD),// ->1181
/*637*/     OPC_Scope, 49|128,1/*177*/, /*->817*/ // 3 children in Scope
/*640*/       OPC_RecordChild0, // #0 = $rA
/*641*/       OPC_MoveChild, 1,
/*643*/       OPC_SwitchOpcode /*2 cases */, 24,  TARGET_OPCODE(PPCISD::Lo),// ->671
/*647*/         OPC_RecordChild0, // #1 = $sym
/*648*/         OPC_MoveChild, 0,
/*650*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetGlobalAddress),
/*653*/         OPC_MoveParent,
/*654*/         OPC_MoveChild, 1,
/*656*/         OPC_CheckInteger, 0, 
/*658*/         OPC_MoveParent,
/*659*/         OPC_MoveParent,
/*660*/         OPC_CheckType, MVT::i32,
/*662*/         OPC_MorphNodeTo, TARGET_OPCODE(PPC::LA), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (add:i32 GPRC:i32:$rA, (PPClo:i32 (tglobaladdr:i32):$sym, 0:i32)) - Complexity = 14
                // Dst: (LA:i32 GPRC:i32:$rA, (tglobaladdr:i32):$sym)
              /*SwitchOpcode*/ 13|128,1/*141*/,  TARGET_OPCODE(PPCISD::Hi),// ->816
/*675*/         OPC_RecordChild0, // #1 = $g
/*676*/         OPC_MoveChild, 0,
/*678*/         OPC_SwitchOpcode /*4 cases */, 31,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->713
/*682*/           OPC_MoveParent,
/*683*/           OPC_MoveChild, 1,
/*685*/           OPC_CheckInteger, 0, 
/*687*/           OPC_MoveParent,
/*688*/           OPC_MoveParent,
/*689*/           OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->701
/*692*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                    // Src: (add:i32 GPRC:i32:$in, (PPChi:i32 (tglobaladdr:i32):$g, 0:i32)) - Complexity = 14
                    // Dst: (ADDIS:i32 GPRC:i32:$in, (tglobaladdr:i32):$g)
                  /*SwitchType*/ 9,  MVT::i64,// ->712
/*703*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                    // Src: (add:i64 G8RC:i64:$in, (PPChi:i64 (tglobaladdr:i64):$g, 0:i64)) - Complexity = 14
                    // Dst: (ADDIS8:i64 G8RC:i64:$in, (tglobaladdr:i64):$g)
                  0, // EndSwitchType
                /*SwitchOpcode*/ 31,  TARGET_OPCODE(ISD::TargetConstantPool),// ->747
/*716*/           OPC_MoveParent,
/*717*/           OPC_MoveChild, 1,
/*719*/           OPC_CheckInteger, 0, 
/*721*/           OPC_MoveParent,
/*722*/           OPC_MoveParent,
/*723*/           OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->735
/*726*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                    // Src: (add:i32 GPRC:i32:$in, (PPChi:i32 (tconstpool:i32):$g, 0:i32)) - Complexity = 14
                    // Dst: (ADDIS:i32 GPRC:i32:$in, (tconstpool:i32):$g)
                  /*SwitchType*/ 9,  MVT::i64,// ->746
/*737*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                    // Src: (add:i64 G8RC:i64:$in, (PPChi:i64 (tconstpool:i64):$g, 0:i64)) - Complexity = 14
                    // Dst: (ADDIS8:i64 G8RC:i64:$in, (tconstpool:i64):$g)
                  0, // EndSwitchType
                /*SwitchOpcode*/ 31,  TARGET_OPCODE(ISD::TargetJumpTable),// ->781
/*750*/           OPC_MoveParent,
/*751*/           OPC_MoveChild, 1,
/*753*/           OPC_CheckInteger, 0, 
/*755*/           OPC_MoveParent,
/*756*/           OPC_MoveParent,
/*757*/           OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->769
/*760*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                    // Src: (add:i32 GPRC:i32:$in, (PPChi:i32 (tjumptable:i32):$g, 0:i32)) - Complexity = 14
                    // Dst: (ADDIS:i32 GPRC:i32:$in, (tjumptable:i32):$g)
                  /*SwitchType*/ 9,  MVT::i64,// ->780
/*771*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                    // Src: (add:i64 G8RC:i64:$in, (PPChi:i64 (tjumptable:i64):$g, 0:i64)) - Complexity = 14
                    // Dst: (ADDIS8:i64 G8RC:i64:$in, (tjumptable:i64):$g)
                  0, // EndSwitchType
                /*SwitchOpcode*/ 31,  TARGET_OPCODE(ISD::TargetBlockAddress),// ->815
/*784*/           OPC_MoveParent,
/*785*/           OPC_MoveChild, 1,
/*787*/           OPC_CheckInteger, 0, 
/*789*/           OPC_MoveParent,
/*790*/           OPC_MoveParent,
/*791*/           OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->803
/*794*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                    // Src: (add:i32 GPRC:i32:$in, (PPChi:i32 (tblockaddress:i32):$g, 0:i32)) - Complexity = 14
                    // Dst: (ADDIS:i32 GPRC:i32:$in, (tblockaddress:i32):$g)
                  /*SwitchType*/ 9,  MVT::i64,// ->814
/*805*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                    // Src: (add:i64 G8RC:i64:$in, (PPChi:i64 (tblockaddress:i64):$g, 0:i64)) - Complexity = 14
                    // Dst: (ADDIS8:i64 G8RC:i64:$in, (tblockaddress:i64):$g)
                  0, // EndSwitchType
                0, // EndSwitchOpcode
              0, // EndSwitchOpcode
/*817*/     /*Scope*/ 53|128,1/*181*/, /*->1000*/
/*819*/       OPC_MoveChild, 0,
/*821*/       OPC_SwitchOpcode /*2 cases */, 25,  TARGET_OPCODE(PPCISD::Lo),// ->850
/*825*/         OPC_RecordChild0, // #0 = $sym
/*826*/         OPC_MoveChild, 0,
/*828*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetGlobalAddress),
/*831*/         OPC_MoveParent,
/*832*/         OPC_MoveChild, 1,
/*834*/         OPC_CheckInteger, 0, 
/*836*/         OPC_MoveParent,
/*837*/         OPC_MoveParent,
/*838*/         OPC_RecordChild1, // #1 = $rA
/*839*/         OPC_CheckType, MVT::i32,
/*841*/         OPC_MorphNodeTo, TARGET_OPCODE(PPC::LA), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (add:i32 (PPClo:i32 (tglobaladdr:i32):$sym, 0:i32), GPRC:i32:$rA) - Complexity = 14
                // Dst: (LA:i32 GPRC:i32:$rA, (tglobaladdr:i32):$sym)
              /*SwitchOpcode*/ 17|128,1/*145*/,  TARGET_OPCODE(PPCISD::Hi),// ->999
/*854*/         OPC_RecordChild0, // #0 = $g
/*855*/         OPC_MoveChild, 0,
/*857*/         OPC_SwitchOpcode /*4 cases */, 32,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->893
/*861*/           OPC_MoveParent,
/*862*/           OPC_MoveChild, 1,
/*864*/           OPC_CheckInteger, 0, 
/*866*/           OPC_MoveParent,
/*867*/           OPC_MoveParent,
/*868*/           OPC_RecordChild1, // #1 = $in
/*869*/           OPC_SwitchType /*2 cases */, 9,  MVT::i64,// ->881
/*872*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                    // Src: (add:i64 (PPChi:i64 (tglobaladdr:i64):$g, 0:i64), G8RC:i64:$in) - Complexity = 14
                    // Dst: (ADDIS8:i64 G8RC:i64:$in, (tglobaladdr:i64):$g)
                  /*SwitchType*/ 9,  MVT::i32,// ->892
/*883*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                    // Src: (add:i32 (PPChi:i32 (tglobaladdr:i32):$g, 0:i32), GPRC:i32:$in) - Complexity = 14
                    // Dst: (ADDIS:i32 GPRC:i32:$in, (tglobaladdr:i32):$g)
                  0, // EndSwitchType
                /*SwitchOpcode*/ 32,  TARGET_OPCODE(ISD::TargetConstantPool),// ->928
/*896*/           OPC_MoveParent,
/*897*/           OPC_MoveChild, 1,
/*899*/           OPC_CheckInteger, 0, 
/*901*/           OPC_MoveParent,
/*902*/           OPC_MoveParent,
/*903*/           OPC_RecordChild1, // #1 = $in
/*904*/           OPC_SwitchType /*2 cases */, 9,  MVT::i64,// ->916
/*907*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                    // Src: (add:i64 (PPChi:i64 (tconstpool:i64):$g, 0:i64), G8RC:i64:$in) - Complexity = 14
                    // Dst: (ADDIS8:i64 G8RC:i64:$in, (tconstpool:i64):$g)
                  /*SwitchType*/ 9,  MVT::i32,// ->927
/*918*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                    // Src: (add:i32 (PPChi:i32 (tconstpool:i32):$g, 0:i32), GPRC:i32:$in) - Complexity = 14
                    // Dst: (ADDIS:i32 GPRC:i32:$in, (tconstpool:i32):$g)
                  0, // EndSwitchType
                /*SwitchOpcode*/ 32,  TARGET_OPCODE(ISD::TargetJumpTable),// ->963
/*931*/           OPC_MoveParent,
/*932*/           OPC_MoveChild, 1,
/*934*/           OPC_CheckInteger, 0, 
/*936*/           OPC_MoveParent,
/*937*/           OPC_MoveParent,
/*938*/           OPC_RecordChild1, // #1 = $in
/*939*/           OPC_SwitchType /*2 cases */, 9,  MVT::i64,// ->951
/*942*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                    // Src: (add:i64 (PPChi:i64 (tjumptable:i64):$g, 0:i64), G8RC:i64:$in) - Complexity = 14
                    // Dst: (ADDIS8:i64 G8RC:i64:$in, (tjumptable:i64):$g)
                  /*SwitchType*/ 9,  MVT::i32,// ->962
/*953*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                    // Src: (add:i32 (PPChi:i32 (tjumptable:i32):$g, 0:i32), GPRC:i32:$in) - Complexity = 14
                    // Dst: (ADDIS:i32 GPRC:i32:$in, (tjumptable:i32):$g)
                  0, // EndSwitchType
                /*SwitchOpcode*/ 32,  TARGET_OPCODE(ISD::TargetBlockAddress),// ->998
/*966*/           OPC_MoveParent,
/*967*/           OPC_MoveChild, 1,
/*969*/           OPC_CheckInteger, 0, 
/*971*/           OPC_MoveParent,
/*972*/           OPC_MoveParent,
/*973*/           OPC_RecordChild1, // #1 = $in
/*974*/           OPC_SwitchType /*2 cases */, 9,  MVT::i64,// ->986
/*977*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                    // Src: (add:i64 (PPChi:i64 (tblockaddress:i64):$g, 0:i64), G8RC:i64:$in) - Complexity = 14
                    // Dst: (ADDIS8:i64 G8RC:i64:$in, (tblockaddress:i64):$g)
                  /*SwitchType*/ 9,  MVT::i32,// ->997
/*988*/             OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                    // Src: (add:i32 (PPChi:i32 (tblockaddress:i32):$g, 0:i32), GPRC:i32:$in) - Complexity = 14
                    // Dst: (ADDIS:i32 GPRC:i32:$in, (tblockaddress:i32):$g)
                  0, // EndSwitchType
                0, // EndSwitchOpcode
              0, // EndSwitchOpcode
/*1000*/    /*Scope*/ 50|128,1/*178*/, /*->1180*/
/*1002*/      OPC_RecordChild0, // #0 = $rA
/*1003*/      OPC_RecordChild1, // #1 = $imm
/*1004*/      OPC_Scope, 113, /*->1119*/ // 6 children in Scope
/*1006*/        OPC_MoveChild, 1,
/*1008*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1011*/        OPC_Scope, 16, /*->1029*/ // 5 children in Scope
/*1013*/          OPC_CheckPredicate, 0, // Predicate_immSExt16
/*1015*/          OPC_MoveParent,
/*1016*/          OPC_CheckType, MVT::i32,
/*1018*/          OPC_EmitConvertToTarget, 1,
/*1020*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDI), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
                  // Src: (add:i32 GPRC:i32:$rA, (imm:i32)<<P:Predicate_immSExt16>>:$imm) - Complexity = 7
                  // Dst: (ADDI:i32 GPRC:i32:$rA, (imm:i32):$imm)
/*1029*/        /*Scope*/ 19, /*->1049*/
/*1030*/          OPC_CheckPredicate, 1, // Predicate_imm16ShiftedSExt
/*1032*/          OPC_MoveParent,
/*1033*/          OPC_CheckType, MVT::i32,
/*1035*/          OPC_EmitConvertToTarget, 1,
/*1037*/          OPC_EmitNodeXForm, 0, 2, // HI16
/*1040*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                  // Src: (add:i32 GPRC:i32:$rA, (imm:i32)<<P:Predicate_imm16ShiftedSExt>><<X:HI16>>:$imm) - Complexity = 7
                  // Dst: (ADDIS:i32 GPRC:i32:$rA, (HI16:i32 (imm:i32):$imm))
/*1049*/        /*Scope*/ 16, /*->1066*/
/*1050*/          OPC_CheckPredicate, 0, // Predicate_immSExt16
/*1052*/          OPC_MoveParent,
/*1053*/          OPC_CheckType, MVT::i64,
/*1055*/          OPC_EmitConvertToTarget, 1,
/*1057*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDI8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (add:i64 G8RC:i64:$rA, (imm:i64)<<P:Predicate_immSExt16>>:$imm) - Complexity = 7
                  // Dst: (ADDI8:i64 G8RC:i64:$rA, (imm:i64):$imm)
/*1066*/        /*Scope*/ 19, /*->1086*/
/*1067*/          OPC_CheckPredicate, 1, // Predicate_imm16ShiftedSExt
/*1069*/          OPC_MoveParent,
/*1070*/          OPC_CheckType, MVT::i64,
/*1072*/          OPC_EmitConvertToTarget, 1,
/*1074*/          OPC_EmitNodeXForm, 0, 2, // HI16
/*1077*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                  // Src: (add:i64 G8RC:i64:$rA, (imm:i64)<<P:Predicate_imm16ShiftedSExt>><<X:HI16>>:$imm) - Complexity = 7
                  // Dst: (ADDIS8:i64 G8RC:i64:$rA, (HI16:i64 (imm:i64):$imm))
/*1086*/        /*Scope*/ 31, /*->1118*/
/*1087*/          OPC_MoveParent,
/*1088*/          OPC_CheckType, MVT::i32,
/*1090*/          OPC_EmitConvertToTarget, 1,
/*1092*/          OPC_EmitNodeXForm, 1, 2, // LO16
/*1095*/          OPC_EmitNode, TARGET_OPCODE(PPC::ADDI), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3,  // Results = #4 
/*1104*/          OPC_EmitConvertToTarget, 1,
/*1106*/          OPC_EmitNodeXForm, 2, 5, // HA16
/*1109*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIS), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 4, 6, 
                  // Src: (add:i32 GPRC:i32:$in, (imm:i32):$imm) - Complexity = 6
                  // Dst: (ADDIS:i32 (ADDI:i32 GPRC:i32:$in, (LO16:i32 (imm:i32):$imm)), (HA16:i32 (imm:i32):$imm))
/*1118*/        0, /*End of Scope*/
/*1119*/      /*Scope*/ 11, /*->1131*/
/*1120*/        OPC_CheckType, MVT::i32,
/*1122*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADD4), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (add:i32 GPRC:i32:$rA, GPRC:i32:$rB) - Complexity = 3
                // Dst: (ADD4:i32 GPRC:i32:$rA, GPRC:i32:$rB)
/*1131*/      /*Scope*/ 11, /*->1143*/
/*1132*/        OPC_CheckType, MVT::i64,
/*1134*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADD8), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (add:i64 G8RC:i64:$rA, G8RC:i64:$rB) - Complexity = 3
                // Dst: (ADD8:i64 G8RC:i64:$rA, G8RC:i64:$rB)
/*1143*/      /*Scope*/ 11, /*->1155*/
/*1144*/        OPC_CheckType, MVT::v16i8,
/*1146*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDUBM), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
                // Src: (add:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 3
                // Dst: (VADDUBM:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*1155*/      /*Scope*/ 11, /*->1167*/
/*1156*/        OPC_CheckType, MVT::v8i16,
/*1158*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDUHM), 0,
                    1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
                // Src: (add:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 3
                // Dst: (VADDUHM:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*1167*/      /*Scope*/ 11, /*->1179*/
/*1168*/        OPC_CheckType, MVT::v4i32,
/*1170*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDUWM), 0,
                    1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
                // Src: (add:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 3
                // Dst: (VADDUWM:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*1179*/      0, /*End of Scope*/
/*1180*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 72|128,6/*840*/,  TARGET_OPCODE(ISD::LOAD),// ->2025
/*1185*/    OPC_RecordMemRef,
/*1186*/    OPC_RecordNode,   // #0 = 'ld' chained node
/*1187*/    OPC_RecordChild1, // #1 = $src
/*1188*/    OPC_CheckPredicate, 2, // Predicate_unindexedload
/*1190*/    OPC_Scope, 19, /*->1211*/ // 18 children in Scope
/*1192*/      OPC_CheckPredicate, 3, // Predicate_zextload
/*1194*/      OPC_CheckPredicate, 4, // Predicate_zextloadi8
/*1196*/      OPC_CheckType, MVT::i32,
/*1198*/      OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1201*/      OPC_EmitMergeInputChains1_0,
/*1202*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZ), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 13
              // Dst: (LBZ:i32 iaddr:iPTR:$src)
/*1211*/    /*Scope*/ 19, /*->1231*/
/*1212*/      OPC_CheckPredicate, 5, // Predicate_sextload
/*1214*/      OPC_CheckPredicate, 6, // Predicate_sextloadi16
/*1216*/      OPC_CheckType, MVT::i32,
/*1218*/      OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1221*/      OPC_EmitMergeInputChains1_0,
/*1222*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHA), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi16>> - Complexity = 13
              // Dst: (LHA:i32 iaddr:iPTR:$src)
/*1231*/    /*Scope*/ 19, /*->1251*/
/*1232*/      OPC_CheckPredicate, 3, // Predicate_zextload
/*1234*/      OPC_CheckPredicate, 7, // Predicate_zextloadi16
/*1236*/      OPC_CheckType, MVT::i32,
/*1238*/      OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1241*/      OPC_EmitMergeInputChains1_0,
/*1242*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHZ), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi16>> - Complexity = 13
              // Dst: (LHZ:i32 iaddr:iPTR:$src)
/*1251*/    /*Scope*/ 17, /*->1269*/
/*1252*/      OPC_CheckPredicate, 8, // Predicate_load
/*1254*/      OPC_CheckType, MVT::i32,
/*1256*/      OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1259*/      OPC_EmitMergeInputChains1_0,
/*1260*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LWZ), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
              // Dst: (LWZ:i32 iaddr:iPTR:$src)
/*1269*/    /*Scope*/ 19, /*->1289*/
/*1270*/      OPC_CheckPredicate, 3, // Predicate_zextload
/*1272*/      OPC_CheckPredicate, 4, // Predicate_zextloadi8
/*1274*/      OPC_CheckType, MVT::i32,
/*1276*/      OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1279*/      OPC_EmitMergeInputChains1_0,
/*1280*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZX), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 13
              // Dst: (LBZX:i32 xaddr:iPTR:$src)
/*1289*/    /*Scope*/ 19, /*->1309*/
/*1290*/      OPC_CheckPredicate, 5, // Predicate_sextload
/*1292*/      OPC_CheckPredicate, 6, // Predicate_sextloadi16
/*1294*/      OPC_CheckType, MVT::i32,
/*1296*/      OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1299*/      OPC_EmitMergeInputChains1_0,
/*1300*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHAX), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi16>> - Complexity = 13
              // Dst: (LHAX:i32 xaddr:iPTR:$src)
/*1309*/    /*Scope*/ 19, /*->1329*/
/*1310*/      OPC_CheckPredicate, 3, // Predicate_zextload
/*1312*/      OPC_CheckPredicate, 7, // Predicate_zextloadi16
/*1314*/      OPC_CheckType, MVT::i32,
/*1316*/      OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1319*/      OPC_EmitMergeInputChains1_0,
/*1320*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHZX), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi16>> - Complexity = 13
              // Dst: (LHZX:i32 xaddr:iPTR:$src)
/*1329*/    /*Scope*/ 17, /*->1347*/
/*1330*/      OPC_CheckPredicate, 8, // Predicate_load
/*1332*/      OPC_CheckType, MVT::i32,
/*1334*/      OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1337*/      OPC_EmitMergeInputChains1_0,
/*1338*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LWZX), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
              // Dst: (LWZX:i32 xaddr:iPTR:$src)
/*1347*/    /*Scope*/ 36, /*->1384*/
/*1348*/      OPC_CheckPredicate, 3, // Predicate_zextload
/*1350*/      OPC_CheckPredicate, 9, // Predicate_zextloadi1
/*1352*/      OPC_CheckType, MVT::i32,
/*1354*/      OPC_Scope, 13, /*->1369*/ // 2 children in Scope
/*1356*/        OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1359*/        OPC_EmitMergeInputChains1_0,
/*1360*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZ), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i32 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi1>> - Complexity = 13
                // Dst: (LBZ:i32 iaddr:iPTR:$src)
/*1369*/      /*Scope*/ 13, /*->1383*/
/*1370*/        OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1373*/        OPC_EmitMergeInputChains1_0,
/*1374*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZX), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i32 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi1>> - Complexity = 13
                // Dst: (LBZX:i32 xaddr:iPTR:$src)
/*1383*/      0, /*End of Scope*/
/*1384*/    /*Scope*/ 105, /*->1490*/
/*1385*/      OPC_CheckPredicate, 10, // Predicate_extload
/*1387*/      OPC_CheckType, MVT::i32,
/*1389*/      OPC_Scope, 32, /*->1423*/ // 3 children in Scope
/*1391*/        OPC_CheckPredicate, 11, // Predicate_extloadi1
/*1393*/        OPC_Scope, 13, /*->1408*/ // 2 children in Scope
/*1395*/          OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1398*/          OPC_EmitMergeInputChains1_0,
/*1399*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZ), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i32 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi1>> - Complexity = 13
                  // Dst: (LBZ:i32 iaddr:iPTR:$src)
/*1408*/        /*Scope*/ 13, /*->1422*/
/*1409*/          OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1412*/          OPC_EmitMergeInputChains1_0,
/*1413*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZX), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i32 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi1>> - Complexity = 13
                  // Dst: (LBZX:i32 xaddr:iPTR:$src)
/*1422*/        0, /*End of Scope*/
/*1423*/      /*Scope*/ 32, /*->1456*/
/*1424*/        OPC_CheckPredicate, 12, // Predicate_extloadi8
/*1426*/        OPC_Scope, 13, /*->1441*/ // 2 children in Scope
/*1428*/          OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1431*/          OPC_EmitMergeInputChains1_0,
/*1432*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZ), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i32 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi8>> - Complexity = 13
                  // Dst: (LBZ:i32 iaddr:iPTR:$src)
/*1441*/        /*Scope*/ 13, /*->1455*/
/*1442*/          OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1445*/          OPC_EmitMergeInputChains1_0,
/*1446*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZX), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i32 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi8>> - Complexity = 13
                  // Dst: (LBZX:i32 xaddr:iPTR:$src)
/*1455*/        0, /*End of Scope*/
/*1456*/      /*Scope*/ 32, /*->1489*/
/*1457*/        OPC_CheckPredicate, 13, // Predicate_extloadi16
/*1459*/        OPC_Scope, 13, /*->1474*/ // 2 children in Scope
/*1461*/          OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1464*/          OPC_EmitMergeInputChains1_0,
/*1465*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHZ), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i32 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi16>> - Complexity = 13
                  // Dst: (LHZ:i32 iaddr:iPTR:$src)
/*1474*/        /*Scope*/ 13, /*->1488*/
/*1475*/          OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1478*/          OPC_EmitMergeInputChains1_0,
/*1479*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHZX), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i32 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi16>> - Complexity = 13
                  // Dst: (LHZX:i32 xaddr:iPTR:$src)
/*1488*/        0, /*End of Scope*/
/*1489*/      0, /*End of Scope*/
/*1490*/    /*Scope*/ 70, /*->1561*/
/*1491*/      OPC_CheckPredicate, 5, // Predicate_sextload
/*1493*/      OPC_CheckType, MVT::i64,
/*1495*/      OPC_Scope, 15, /*->1512*/ // 4 children in Scope
/*1497*/        OPC_CheckPredicate, 6, // Predicate_sextloadi16
/*1499*/        OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1502*/        OPC_EmitMergeInputChains1_0,
/*1503*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHA8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi16>> - Complexity = 13
                // Dst: (LHA8:i64 iaddr:iPTR:$src)
/*1512*/      /*Scope*/ 15, /*->1528*/
/*1513*/        OPC_CheckPredicate, 14, // Predicate_sextloadi32
/*1515*/        OPC_CheckComplexPat, /*CP*/3, /*#*/1, // SelectAddrImmShift:$src #2 #3
/*1518*/        OPC_EmitMergeInputChains1_0,
/*1519*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LWA), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 ixaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi32>> - Complexity = 13
                // Dst: (LWA:i64 ixaddr:iPTR:$src)
/*1528*/      /*Scope*/ 15, /*->1544*/
/*1529*/        OPC_CheckPredicate, 6, // Predicate_sextloadi16
/*1531*/        OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1534*/        OPC_EmitMergeInputChains1_0,
/*1535*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHAX8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi16>> - Complexity = 13
                // Dst: (LHAX8:i64 xaddr:iPTR:$src)
/*1544*/      /*Scope*/ 15, /*->1560*/
/*1545*/        OPC_CheckPredicate, 14, // Predicate_sextloadi32
/*1547*/        OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1550*/        OPC_EmitMergeInputChains1_0,
/*1551*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LWAX), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi32>> - Complexity = 13
                // Dst: (LWAX:i64 xaddr:iPTR:$src)
/*1560*/      0, /*End of Scope*/
/*1561*/    /*Scope*/ 102, /*->1664*/
/*1562*/      OPC_CheckPredicate, 3, // Predicate_zextload
/*1564*/      OPC_CheckType, MVT::i64,
/*1566*/      OPC_Scope, 15, /*->1583*/ // 6 children in Scope
/*1568*/        OPC_CheckPredicate, 4, // Predicate_zextloadi8
/*1570*/        OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1573*/        OPC_EmitMergeInputChains1_0,
/*1574*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZ8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 13
                // Dst: (LBZ8:i64 iaddr:iPTR:$src)
/*1583*/      /*Scope*/ 15, /*->1599*/
/*1584*/        OPC_CheckPredicate, 7, // Predicate_zextloadi16
/*1586*/        OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1589*/        OPC_EmitMergeInputChains1_0,
/*1590*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHZ8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi16>> - Complexity = 13
                // Dst: (LHZ8:i64 iaddr:iPTR:$src)
/*1599*/      /*Scope*/ 15, /*->1615*/
/*1600*/        OPC_CheckPredicate, 15, // Predicate_zextloadi32
/*1602*/        OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1605*/        OPC_EmitMergeInputChains1_0,
/*1606*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LWZ8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi32>> - Complexity = 13
                // Dst: (LWZ8:i64 iaddr:iPTR:$src)
/*1615*/      /*Scope*/ 15, /*->1631*/
/*1616*/        OPC_CheckPredicate, 4, // Predicate_zextloadi8
/*1618*/        OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1621*/        OPC_EmitMergeInputChains1_0,
/*1622*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZX8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 13
                // Dst: (LBZX8:i64 xaddr:iPTR:$src)
/*1631*/      /*Scope*/ 15, /*->1647*/
/*1632*/        OPC_CheckPredicate, 7, // Predicate_zextloadi16
/*1634*/        OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1637*/        OPC_EmitMergeInputChains1_0,
/*1638*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHZX8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi16>> - Complexity = 13
                // Dst: (LHZX8:i64 xaddr:iPTR:$src)
/*1647*/      /*Scope*/ 15, /*->1663*/
/*1648*/        OPC_CheckPredicate, 15, // Predicate_zextloadi32
/*1650*/        OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1653*/        OPC_EmitMergeInputChains1_0,
/*1654*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LWZX8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi32>> - Complexity = 13
                // Dst: (LWZX8:i64 xaddr:iPTR:$src)
/*1663*/      0, /*End of Scope*/
/*1664*/    /*Scope*/ 34, /*->1699*/
/*1665*/      OPC_CheckPredicate, 8, // Predicate_load
/*1667*/      OPC_CheckType, MVT::i64,
/*1669*/      OPC_Scope, 13, /*->1684*/ // 2 children in Scope
/*1671*/        OPC_CheckComplexPat, /*CP*/3, /*#*/1, // SelectAddrImmShift:$src #2 #3
/*1674*/        OPC_EmitMergeInputChains1_0,
/*1675*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LD), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 ixaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
                // Dst: (LD:i64 ixaddr:iPTR:$src)
/*1684*/      /*Scope*/ 13, /*->1698*/
/*1685*/        OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1688*/        OPC_EmitMergeInputChains1_0,
/*1689*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LDX), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
                // Dst: (LDX:i64 xaddr:iPTR:$src)
/*1698*/      0, /*End of Scope*/
/*1699*/    /*Scope*/ 36, /*->1736*/
/*1700*/      OPC_CheckPredicate, 3, // Predicate_zextload
/*1702*/      OPC_CheckPredicate, 9, // Predicate_zextloadi1
/*1704*/      OPC_CheckType, MVT::i64,
/*1706*/      OPC_Scope, 13, /*->1721*/ // 2 children in Scope
/*1708*/        OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1711*/        OPC_EmitMergeInputChains1_0,
/*1712*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZ8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi1>> - Complexity = 13
                // Dst: (LBZ8:i64 iaddr:iPTR:$src)
/*1721*/      /*Scope*/ 13, /*->1735*/
/*1722*/        OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1725*/        OPC_EmitMergeInputChains1_0,
/*1726*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZX8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi1>> - Complexity = 13
                // Dst: (LBZX8:i64 xaddr:iPTR:$src)
/*1735*/      0, /*End of Scope*/
/*1736*/    /*Scope*/ 10|128,1/*138*/, /*->1876*/
/*1738*/      OPC_CheckPredicate, 10, // Predicate_extload
/*1740*/      OPC_CheckType, MVT::i64,
/*1742*/      OPC_Scope, 32, /*->1776*/ // 4 children in Scope
/*1744*/        OPC_CheckPredicate, 11, // Predicate_extloadi1
/*1746*/        OPC_Scope, 13, /*->1761*/ // 2 children in Scope
/*1748*/          OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1751*/          OPC_EmitMergeInputChains1_0,
/*1752*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZ8), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi1>> - Complexity = 13
                  // Dst: (LBZ8:i64 iaddr:iPTR:$src)
/*1761*/        /*Scope*/ 13, /*->1775*/
/*1762*/          OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1765*/          OPC_EmitMergeInputChains1_0,
/*1766*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZX8), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi1>> - Complexity = 13
                  // Dst: (LBZX8:i64 xaddr:iPTR:$src)
/*1775*/        0, /*End of Scope*/
/*1776*/      /*Scope*/ 32, /*->1809*/
/*1777*/        OPC_CheckPredicate, 12, // Predicate_extloadi8
/*1779*/        OPC_Scope, 13, /*->1794*/ // 2 children in Scope
/*1781*/          OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1784*/          OPC_EmitMergeInputChains1_0,
/*1785*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZ8), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi8>> - Complexity = 13
                  // Dst: (LBZ8:i64 iaddr:iPTR:$src)
/*1794*/        /*Scope*/ 13, /*->1808*/
/*1795*/          OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1798*/          OPC_EmitMergeInputChains1_0,
/*1799*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LBZX8), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi8>> - Complexity = 13
                  // Dst: (LBZX8:i64 xaddr:iPTR:$src)
/*1808*/        0, /*End of Scope*/
/*1809*/      /*Scope*/ 32, /*->1842*/
/*1810*/        OPC_CheckPredicate, 13, // Predicate_extloadi16
/*1812*/        OPC_Scope, 13, /*->1827*/ // 2 children in Scope
/*1814*/          OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1817*/          OPC_EmitMergeInputChains1_0,
/*1818*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHZ8), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi16>> - Complexity = 13
                  // Dst: (LHZ8:i64 iaddr:iPTR:$src)
/*1827*/        /*Scope*/ 13, /*->1841*/
/*1828*/          OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1831*/          OPC_EmitMergeInputChains1_0,
/*1832*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHZX8), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi16>> - Complexity = 13
                  // Dst: (LHZX8:i64 xaddr:iPTR:$src)
/*1841*/        0, /*End of Scope*/
/*1842*/      /*Scope*/ 32, /*->1875*/
/*1843*/        OPC_CheckPredicate, 16, // Predicate_extloadi32
/*1845*/        OPC_Scope, 13, /*->1860*/ // 2 children in Scope
/*1847*/          OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1850*/          OPC_EmitMergeInputChains1_0,
/*1851*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LWZ8), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi32>> - Complexity = 13
                  // Dst: (LWZ8:i64 iaddr:iPTR:$src)
/*1860*/        /*Scope*/ 13, /*->1874*/
/*1861*/          OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1864*/          OPC_EmitMergeInputChains1_0,
/*1865*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LWZX8), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:i64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi32>> - Complexity = 13
                  // Dst: (LWZX8:i64 xaddr:iPTR:$src)
/*1874*/        0, /*End of Scope*/
/*1875*/      0, /*End of Scope*/
/*1876*/    /*Scope*/ 68, /*->1945*/
/*1877*/      OPC_CheckPredicate, 8, // Predicate_load
/*1879*/      OPC_SwitchType /*2 cases */, 30,  MVT::f32,// ->1912
/*1882*/        OPC_Scope, 13, /*->1897*/ // 2 children in Scope
/*1884*/          OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1887*/          OPC_EmitMergeInputChains1_0,
/*1888*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LFS), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::f32, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:f32 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
                  // Dst: (LFS:f32 iaddr:iPTR:$src)
/*1897*/        /*Scope*/ 13, /*->1911*/
/*1898*/          OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1901*/          OPC_EmitMergeInputChains1_0,
/*1902*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LFSX), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::f32, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:f32 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
                  // Dst: (LFSX:f32 xaddr:iPTR:$src)
/*1911*/        0, /*End of Scope*/
              /*SwitchType*/ 30,  MVT::f64,// ->1944
/*1914*/        OPC_Scope, 13, /*->1929*/ // 2 children in Scope
/*1916*/          OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1919*/          OPC_EmitMergeInputChains1_0,
/*1920*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LFD), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::f64, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:f64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
                  // Dst: (LFD:f64 iaddr:iPTR:$src)
/*1929*/        /*Scope*/ 13, /*->1943*/
/*1930*/          OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1933*/          OPC_EmitMergeInputChains1_0,
/*1934*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::LFDX), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::f64, 2/*#Ops*/, 2, 3, 
                  // Src: (ld:f64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
                  // Dst: (LFDX:f64 xaddr:iPTR:$src)
/*1943*/        0, /*End of Scope*/
              0, // EndSwitchType
/*1945*/    /*Scope*/ 60, /*->2006*/
/*1946*/      OPC_CheckPredicate, 10, // Predicate_extload
/*1948*/      OPC_CheckPredicate, 17, // Predicate_extloadf32
/*1950*/      OPC_CheckType, MVT::f64,
/*1952*/      OPC_Scope, 25, /*->1979*/ // 2 children in Scope
/*1954*/        OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectAddrImm:$src #2 #3
/*1957*/        OPC_EmitMergeInputChains1_0,
/*1958*/        OPC_EmitNode, TARGET_OPCODE(PPC::LFS), 0|OPFL_Chain,
                    1/*#VTs*/, MVT::f32, 2/*#Ops*/, 2, 3,  // Results = #4 
/*1967*/        OPC_EmitInteger, MVT::i32, PPC::F8RCRegClassID,
/*1970*/        OPC_MorphNodeTo, TARGET_OPCODE(TargetOpcode::COPY_TO_REGCLASS), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 4, 5, 
                // Src: (ld:f64 iaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadf32>> - Complexity = 13
                // Dst: (COPY_TO_REGCLASS:f64 (LFS:f32 iaddr:iPTR:$src), F8RC:f64)
/*1979*/      /*Scope*/ 25, /*->2005*/
/*1980*/        OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*1983*/        OPC_EmitMergeInputChains1_0,
/*1984*/        OPC_EmitNode, TARGET_OPCODE(PPC::LFSX), 0|OPFL_Chain,
                    1/*#VTs*/, MVT::f32, 2/*#Ops*/, 2, 3,  // Results = #4 
/*1993*/        OPC_EmitInteger, MVT::i32, PPC::F8RCRegClassID,
/*1996*/        OPC_MorphNodeTo, TARGET_OPCODE(TargetOpcode::COPY_TO_REGCLASS), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 4, 5, 
                // Src: (ld:f64 xaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadf32>> - Complexity = 13
                // Dst: (COPY_TO_REGCLASS:f64 (LFSX:f32 xaddr:iPTR:$src), F8RC:f64)
/*2005*/      0, /*End of Scope*/
/*2006*/    /*Scope*/ 17, /*->2024*/
/*2007*/      OPC_CheckPredicate, 8, // Predicate_load
/*2009*/      OPC_CheckType, MVT::v4i32,
/*2011*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$src #2 #3
/*2014*/      OPC_EmitMergeInputChains1_0,
/*2015*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LVX), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:v4i32 xoaddr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
              // Dst: (LVX:v4i32 xoaddr:iPTR:$src)
/*2024*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 29|128,4/*541*/,  TARGET_OPCODE(ISD::STORE),// ->2570
/*2029*/    OPC_RecordMemRef,
/*2030*/    OPC_RecordNode,   // #0 = 'st' chained node
/*2031*/    OPC_RecordChild1, // #1 = $rS
/*2032*/    OPC_Scope, 52|128,1/*180*/, /*->2215*/ // 5 children in Scope
/*2035*/      OPC_CheckChild1Type, MVT::i32,
/*2037*/      OPC_RecordChild2, // #2 = $src
/*2038*/      OPC_Scope, 110, /*->2150*/ // 2 children in Scope
/*2040*/        OPC_CheckPredicate, 18, // Predicate_unindexedstore
/*2042*/        OPC_Scope, 36, /*->2080*/ // 4 children in Scope
/*2044*/          OPC_CheckPredicate, 19, // Predicate_truncstore
/*2046*/          OPC_Scope, 15, /*->2063*/ // 2 children in Scope
/*2048*/            OPC_CheckPredicate, 20, // Predicate_truncstorei8
/*2050*/            OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectAddrImm:$src #3 #4
/*2053*/            OPC_EmitMergeInputChains1_0,
/*2054*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STB), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st GPRC:i32:$rS, iaddr:iPTR:$src)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei8>> - Complexity = 13
                    // Dst: (STB GPRC:i32:$rS, iaddr:iPTR:$src)
/*2063*/          /*Scope*/ 15, /*->2079*/
/*2064*/            OPC_CheckPredicate, 21, // Predicate_truncstorei16
/*2066*/            OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectAddrImm:$src #3 #4
/*2069*/            OPC_EmitMergeInputChains1_0,
/*2070*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STH), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st GPRC:i32:$rS, iaddr:iPTR:$src)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 13
                    // Dst: (STH GPRC:i32:$rS, iaddr:iPTR:$src)
/*2079*/          0, /*End of Scope*/
/*2080*/        /*Scope*/ 15, /*->2096*/
/*2081*/          OPC_CheckPredicate, 22, // Predicate_store
/*2083*/          OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectAddrImm:$src #3 #4
/*2086*/          OPC_EmitMergeInputChains1_0,
/*2087*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::STW), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                  // Src: (st GPRC:i32:$rS, iaddr:iPTR:$src)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                  // Dst: (STW GPRC:i32:$rS, iaddr:iPTR:$src)
/*2096*/        /*Scope*/ 36, /*->2133*/
/*2097*/          OPC_CheckPredicate, 19, // Predicate_truncstore
/*2099*/          OPC_Scope, 15, /*->2116*/ // 2 children in Scope
/*2101*/            OPC_CheckPredicate, 20, // Predicate_truncstorei8
/*2103*/            OPC_CheckComplexPat, /*CP*/2, /*#*/2, // SelectAddrIdx:$dst #3 #4
/*2106*/            OPC_EmitMergeInputChains1_0,
/*2107*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STBX), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st GPRC:i32:$rS, xaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei8>> - Complexity = 13
                    // Dst: (STBX GPRC:i32:$rS, xaddr:iPTR:$dst)
/*2116*/          /*Scope*/ 15, /*->2132*/
/*2117*/            OPC_CheckPredicate, 21, // Predicate_truncstorei16
/*2119*/            OPC_CheckComplexPat, /*CP*/2, /*#*/2, // SelectAddrIdx:$dst #3 #4
/*2122*/            OPC_EmitMergeInputChains1_0,
/*2123*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STHX), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st GPRC:i32:$rS, xaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 13
                    // Dst: (STHX GPRC:i32:$rS, xaddr:iPTR:$dst)
/*2132*/          0, /*End of Scope*/
/*2133*/        /*Scope*/ 15, /*->2149*/
/*2134*/          OPC_CheckPredicate, 22, // Predicate_store
/*2136*/          OPC_CheckComplexPat, /*CP*/2, /*#*/2, // SelectAddrIdx:$dst #3 #4
/*2139*/          OPC_EmitMergeInputChains1_0,
/*2140*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::STWX), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                  // Src: (st GPRC:i32:$rS, xaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                  // Dst: (STWX GPRC:i32:$rS, xaddr:iPTR:$dst)
/*2149*/        0, /*End of Scope*/
/*2150*/      /*Scope*/ 63, /*->2214*/
/*2151*/        OPC_RecordChild3, // #3 = $ptroff
/*2152*/        OPC_Scope, 40, /*->2194*/ // 2 children in Scope
/*2154*/          OPC_CheckPredicate, 23, // Predicate_itruncstore
/*2156*/          OPC_CheckPredicate, 24, // Predicate_pre_truncst
/*2158*/          OPC_Scope, 16, /*->2176*/ // 2 children in Scope
/*2160*/            OPC_CheckPredicate, 25, // Predicate_pre_truncsti8
/*2162*/            OPC_CheckComplexPat, /*CP*/4, /*#*/3, // SelectAddrImmOffs:$ptroff #4
/*2165*/            OPC_EmitMergeInputChains1_0,
/*2166*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STBU), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::iPTR, 3/*#Ops*/, 1, 4, 2, 
                    // Src: (ist:iPTR GPRC:i32:$rS, ptr_rc:iPTR:$ptrreg, iaddroff:iPTR:$ptroff)<<P:Predicate_itruncstore>><<P:Predicate_pre_truncst>><<P:Predicate_pre_truncsti8>> - Complexity = 10
                    // Dst: (STBU:iPTR GPRC:i32:$rS, iaddroff:i32:$ptroff, ptr_rc:iPTR:$ptrreg)
/*2176*/          /*Scope*/ 16, /*->2193*/
/*2177*/            OPC_CheckPredicate, 26, // Predicate_pre_truncsti16
/*2179*/            OPC_CheckComplexPat, /*CP*/4, /*#*/3, // SelectAddrImmOffs:$ptroff #4
/*2182*/            OPC_EmitMergeInputChains1_0,
/*2183*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STHU), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::iPTR, 3/*#Ops*/, 1, 4, 2, 
                    // Src: (ist:iPTR GPRC:i32:$rS, ptr_rc:iPTR:$ptrreg, iaddroff:iPTR:$ptroff)<<P:Predicate_itruncstore>><<P:Predicate_pre_truncst>><<P:Predicate_pre_truncsti16>> - Complexity = 10
                    // Dst: (STHU:iPTR GPRC:i32:$rS, iaddroff:i32:$ptroff, ptr_rc:iPTR:$ptrreg)
/*2193*/          0, /*End of Scope*/
/*2194*/        /*Scope*/ 18, /*->2213*/
/*2195*/          OPC_CheckPredicate, 27, // Predicate_istore
/*2197*/          OPC_CheckPredicate, 28, // Predicate_pre_store
/*2199*/          OPC_CheckComplexPat, /*CP*/4, /*#*/3, // SelectAddrImmOffs:$ptroff #4
/*2202*/          OPC_EmitMergeInputChains1_0,
/*2203*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::STWU), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::iPTR, 3/*#Ops*/, 1, 4, 2, 
                  // Src: (ist:iPTR GPRC:i32:$rS, ptr_rc:iPTR:$ptrreg, iaddroff:iPTR:$ptroff)<<P:Predicate_istore>><<P:Predicate_pre_store>> - Complexity = 10
                  // Dst: (STWU:iPTR GPRC:i32:$rS, iaddroff:i32:$ptroff, ptr_rc:iPTR:$ptrreg)
/*2213*/        0, /*End of Scope*/
/*2214*/      0, /*End of Scope*/
/*2215*/    /*Scope*/ 60, /*->2276*/
/*2216*/      OPC_CheckChild1Type, MVT::f32,
/*2218*/      OPC_RecordChild2, // #2 = $dst
/*2219*/      OPC_Scope, 34, /*->2255*/ // 2 children in Scope
/*2221*/        OPC_CheckPredicate, 18, // Predicate_unindexedstore
/*2223*/        OPC_CheckPredicate, 22, // Predicate_store
/*2225*/        OPC_Scope, 13, /*->2240*/ // 2 children in Scope
/*2227*/          OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectAddrImm:$dst #3 #4
/*2230*/          OPC_EmitMergeInputChains1_0,
/*2231*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::STFS), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                  // Src: (st F4RC:f32:$rS, iaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                  // Dst: (STFS F4RC:f32:$rS, iaddr:iPTR:$dst)
/*2240*/        /*Scope*/ 13, /*->2254*/
/*2241*/          OPC_CheckComplexPat, /*CP*/2, /*#*/2, // SelectAddrIdx:$dst #3 #4
/*2244*/          OPC_EmitMergeInputChains1_0,
/*2245*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::STFSX), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                  // Src: (st F4RC:f32:$frS, xaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                  // Dst: (STFSX F4RC:f32:$frS, xaddr:iPTR:$dst)
/*2254*/        0, /*End of Scope*/
/*2255*/      /*Scope*/ 19, /*->2275*/
/*2256*/        OPC_RecordChild3, // #3 = $ptroff
/*2257*/        OPC_CheckPredicate, 27, // Predicate_istore
/*2259*/        OPC_CheckPredicate, 28, // Predicate_pre_store
/*2261*/        OPC_CheckComplexPat, /*CP*/4, /*#*/3, // SelectAddrImmOffs:$ptroff #4
/*2264*/        OPC_EmitMergeInputChains1_0,
/*2265*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::STFSU), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::iPTR, 3/*#Ops*/, 1, 4, 2, 
                // Src: (ist:iPTR F4RC:f32:$rS, ptr_rc:iPTR:$ptrreg, iaddroff:iPTR:$ptroff)<<P:Predicate_istore>><<P:Predicate_pre_store>> - Complexity = 10
                // Dst: (STFSU:iPTR F4RC:f32:$rS, iaddroff:i32:$ptroff, ptr_rc:iPTR:$ptrreg)
/*2275*/      0, /*End of Scope*/
/*2276*/    /*Scope*/ 60, /*->2337*/
/*2277*/      OPC_CheckChild1Type, MVT::f64,
/*2279*/      OPC_RecordChild2, // #2 = $dst
/*2280*/      OPC_Scope, 34, /*->2316*/ // 2 children in Scope
/*2282*/        OPC_CheckPredicate, 18, // Predicate_unindexedstore
/*2284*/        OPC_CheckPredicate, 22, // Predicate_store
/*2286*/        OPC_Scope, 13, /*->2301*/ // 2 children in Scope
/*2288*/          OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectAddrImm:$dst #3 #4
/*2291*/          OPC_EmitMergeInputChains1_0,
/*2292*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::STFD), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                  // Src: (st F8RC:f64:$rS, iaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                  // Dst: (STFD F8RC:f64:$rS, iaddr:iPTR:$dst)
/*2301*/        /*Scope*/ 13, /*->2315*/
/*2302*/          OPC_CheckComplexPat, /*CP*/2, /*#*/2, // SelectAddrIdx:$dst #3 #4
/*2305*/          OPC_EmitMergeInputChains1_0,
/*2306*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::STFDX), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                  // Src: (st F8RC:f64:$frS, xaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                  // Dst: (STFDX F8RC:f64:$frS, xaddr:iPTR:$dst)
/*2315*/        0, /*End of Scope*/
/*2316*/      /*Scope*/ 19, /*->2336*/
/*2317*/        OPC_RecordChild3, // #3 = $ptroff
/*2318*/        OPC_CheckPredicate, 27, // Predicate_istore
/*2320*/        OPC_CheckPredicate, 28, // Predicate_pre_store
/*2322*/        OPC_CheckComplexPat, /*CP*/4, /*#*/3, // SelectAddrImmOffs:$ptroff #4
/*2325*/        OPC_EmitMergeInputChains1_0,
/*2326*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::STFDU), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::iPTR, 3/*#Ops*/, 1, 4, 2, 
                // Src: (ist:iPTR F8RC:f64:$rS, ptr_rc:iPTR:$ptrreg, iaddroff:iPTR:$ptroff)<<P:Predicate_istore>><<P:Predicate_pre_store>> - Complexity = 10
                // Dst: (STFDU:iPTR F8RC:f64:$rS, iaddroff:i32:$ptroff, ptr_rc:iPTR:$ptrreg)
/*2336*/      0, /*End of Scope*/
/*2337*/    /*Scope*/ 20, /*->2358*/
/*2338*/      OPC_CheckChild1Type, MVT::v4i32,
/*2340*/      OPC_RecordChild2, // #2 = $dst
/*2341*/      OPC_CheckPredicate, 18, // Predicate_unindexedstore
/*2343*/      OPC_CheckPredicate, 22, // Predicate_store
/*2345*/      OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*2348*/      OPC_EmitMergeInputChains1_0,
/*2349*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::STVX), 0|OPFL_Chain|OPFL_MemRefs,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
              // Src: (st VRRC:v4i32:$rS, xoaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
              // Dst: (STVX VRRC:v4i32:$rS, xoaddr:iPTR:$dst)
/*2358*/    /*Scope*/ 81|128,1/*209*/, /*->2569*/
/*2360*/      OPC_CheckChild1Type, MVT::i64,
/*2362*/      OPC_RecordChild2, // #2 = $src
/*2363*/      OPC_Scope, 10|128,1/*138*/, /*->2504*/ // 2 children in Scope
/*2366*/        OPC_CheckPredicate, 18, // Predicate_unindexedstore
/*2368*/        OPC_Scope, 100, /*->2470*/ // 2 children in Scope
/*2370*/          OPC_CheckPredicate, 19, // Predicate_truncstore
/*2372*/          OPC_Scope, 15, /*->2389*/ // 6 children in Scope
/*2374*/            OPC_CheckPredicate, 20, // Predicate_truncstorei8
/*2376*/            OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectAddrImm:$src #3 #4
/*2379*/            OPC_EmitMergeInputChains1_0,
/*2380*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STB8), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st G8RC:i64:$rS, iaddr:iPTR:$src)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei8>> - Complexity = 13
                    // Dst: (STB8 G8RC:i64:$rS, iaddr:iPTR:$src)
/*2389*/          /*Scope*/ 15, /*->2405*/
/*2390*/            OPC_CheckPredicate, 21, // Predicate_truncstorei16
/*2392*/            OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectAddrImm:$src #3 #4
/*2395*/            OPC_EmitMergeInputChains1_0,
/*2396*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STH8), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st G8RC:i64:$rS, iaddr:iPTR:$src)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 13
                    // Dst: (STH8 G8RC:i64:$rS, iaddr:iPTR:$src)
/*2405*/          /*Scope*/ 15, /*->2421*/
/*2406*/            OPC_CheckPredicate, 29, // Predicate_truncstorei32
/*2408*/            OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectAddrImm:$src #3 #4
/*2411*/            OPC_EmitMergeInputChains1_0,
/*2412*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STW8), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st G8RC:i64:$rS, iaddr:iPTR:$src)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei32>> - Complexity = 13
                    // Dst: (STW8 G8RC:i64:$rS, iaddr:iPTR:$src)
/*2421*/          /*Scope*/ 15, /*->2437*/
/*2422*/            OPC_CheckPredicate, 20, // Predicate_truncstorei8
/*2424*/            OPC_CheckComplexPat, /*CP*/2, /*#*/2, // SelectAddrIdx:$dst #3 #4
/*2427*/            OPC_EmitMergeInputChains1_0,
/*2428*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STBX8), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st G8RC:i64:$rS, xaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei8>> - Complexity = 13
                    // Dst: (STBX8 G8RC:i64:$rS, xaddr:iPTR:$dst)
/*2437*/          /*Scope*/ 15, /*->2453*/
/*2438*/            OPC_CheckPredicate, 21, // Predicate_truncstorei16
/*2440*/            OPC_CheckComplexPat, /*CP*/2, /*#*/2, // SelectAddrIdx:$dst #3 #4
/*2443*/            OPC_EmitMergeInputChains1_0,
/*2444*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STHX8), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st G8RC:i64:$rS, xaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 13
                    // Dst: (STHX8 G8RC:i64:$rS, xaddr:iPTR:$dst)
/*2453*/          /*Scope*/ 15, /*->2469*/
/*2454*/            OPC_CheckPredicate, 29, // Predicate_truncstorei32
/*2456*/            OPC_CheckComplexPat, /*CP*/2, /*#*/2, // SelectAddrIdx:$dst #3 #4
/*2459*/            OPC_EmitMergeInputChains1_0,
/*2460*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STWX8), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st G8RC:i64:$rS, xaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei32>> - Complexity = 13
                    // Dst: (STWX8 G8RC:i64:$rS, xaddr:iPTR:$dst)
/*2469*/          0, /*End of Scope*/
/*2470*/        /*Scope*/ 32, /*->2503*/
/*2471*/          OPC_CheckPredicate, 22, // Predicate_store
/*2473*/          OPC_Scope, 13, /*->2488*/ // 2 children in Scope
/*2475*/            OPC_CheckComplexPat, /*CP*/3, /*#*/2, // SelectAddrImmShift:$dst #3 #4
/*2478*/            OPC_EmitMergeInputChains1_0,
/*2479*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STD), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st G8RC:i64:$rS, ixaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                    // Dst: (STD G8RC:i64:$rS, ixaddr:iPTR:$dst)
/*2488*/          /*Scope*/ 13, /*->2502*/
/*2489*/            OPC_CheckComplexPat, /*CP*/2, /*#*/2, // SelectAddrIdx:$dst #3 #4
/*2492*/            OPC_EmitMergeInputChains1_0,
/*2493*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STDX), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st G8RC:i64:$rS, xaddr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                    // Dst: (STDX G8RC:i64:$rS, xaddr:iPTR:$dst)
/*2502*/          0, /*End of Scope*/
/*2503*/        0, /*End of Scope*/
/*2504*/      /*Scope*/ 63, /*->2568*/
/*2505*/        OPC_RecordChild3, // #3 = $ptroff
/*2506*/        OPC_Scope, 40, /*->2548*/ // 2 children in Scope
/*2508*/          OPC_CheckPredicate, 23, // Predicate_itruncstore
/*2510*/          OPC_CheckPredicate, 24, // Predicate_pre_truncst
/*2512*/          OPC_Scope, 16, /*->2530*/ // 2 children in Scope
/*2514*/            OPC_CheckPredicate, 25, // Predicate_pre_truncsti8
/*2516*/            OPC_CheckComplexPat, /*CP*/4, /*#*/3, // SelectAddrImmOffs:$ptroff #4
/*2519*/            OPC_EmitMergeInputChains1_0,
/*2520*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STBU8), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::iPTR, 3/*#Ops*/, 1, 4, 2, 
                    // Src: (ist:iPTR G8RC:i64:$rS, ptr_rc:iPTR:$ptrreg, iaddroff:iPTR:$ptroff)<<P:Predicate_itruncstore>><<P:Predicate_pre_truncst>><<P:Predicate_pre_truncsti8>> - Complexity = 10
                    // Dst: (STBU8:iPTR G8RC:i64:$rS, iaddroff:i32:$ptroff, ptr_rc:iPTR:$ptrreg)
/*2530*/          /*Scope*/ 16, /*->2547*/
/*2531*/            OPC_CheckPredicate, 26, // Predicate_pre_truncsti16
/*2533*/            OPC_CheckComplexPat, /*CP*/4, /*#*/3, // SelectAddrImmOffs:$ptroff #4
/*2536*/            OPC_EmitMergeInputChains1_0,
/*2537*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::STHU8), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::iPTR, 3/*#Ops*/, 1, 4, 2, 
                    // Src: (ist:iPTR G8RC:i64:$rS, ptr_rc:iPTR:$ptrreg, iaddroff:iPTR:$ptroff)<<P:Predicate_itruncstore>><<P:Predicate_pre_truncst>><<P:Predicate_pre_truncsti16>> - Complexity = 10
                    // Dst: (STHU8:iPTR G8RC:i64:$rS, iaddroff:i32:$ptroff, ptr_rc:iPTR:$ptrreg)
/*2547*/          0, /*End of Scope*/
/*2548*/        /*Scope*/ 18, /*->2567*/
/*2549*/          OPC_CheckPredicate, 27, // Predicate_istore
/*2551*/          OPC_CheckPredicate, 28, // Predicate_pre_store
/*2553*/          OPC_CheckComplexPat, /*CP*/4, /*#*/3, // SelectAddrImmOffs:$ptroff #4
/*2556*/          OPC_EmitMergeInputChains1_0,
/*2557*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::STDU), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::iPTR, 3/*#Ops*/, 1, 4, 2, 
                  // Src: (ist:iPTR G8RC:i64:$rS, ptr_rc:iPTR:$ptrreg, iaddroff:iPTR:$ptroff)<<P:Predicate_istore>><<P:Predicate_pre_store>> - Complexity = 10
                  // Dst: (STDU:iPTR G8RC:i64:$rS, iaddroff:i32:$ptroff, ptr_rc:iPTR:$ptrreg)
/*2567*/        0, /*End of Scope*/
/*2568*/      0, /*End of Scope*/
/*2569*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 79,  TARGET_OPCODE(ISD::ATOMIC_LOAD_ADD),// ->2652
/*2573*/    OPC_RecordMemRef,
/*2574*/    OPC_RecordNode,   // #0 = 'atomic_load_add' chained node
/*2575*/    OPC_RecordChild1, // #1 = $ptr
/*2576*/    OPC_RecordChild2, // #2 = $incr
/*2577*/    OPC_SwitchType /*2 cases */, 53,  MVT::i32,// ->2633
/*2580*/      OPC_Scope, 16, /*->2598*/ // 3 children in Scope
/*2582*/        OPC_CheckPredicate, 30, // Predicate_atomic_load_add_8
/*2584*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2587*/        OPC_EmitMergeInputChains1_0,
/*2588*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_ADD_I8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_add:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_add_8>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_ADD_I8:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2598*/      /*Scope*/ 16, /*->2615*/
/*2599*/        OPC_CheckPredicate, 31, // Predicate_atomic_load_add_16
/*2601*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2604*/        OPC_EmitMergeInputChains1_0,
/*2605*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_ADD_I16), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_add:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_add_16>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_ADD_I16:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2615*/      /*Scope*/ 16, /*->2632*/
/*2616*/        OPC_CheckPredicate, 32, // Predicate_atomic_load_add_32
/*2618*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2621*/        OPC_EmitMergeInputChains1_0,
/*2622*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_ADD_I32), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_add:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_add_32>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_ADD_I32:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2632*/      0, /*End of Scope*/
            /*SwitchType*/ 16,  MVT::i64,// ->2651
/*2635*/      OPC_CheckPredicate, 33, // Predicate_atomic_load_add_64
/*2637*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2640*/      OPC_EmitMergeInputChains1_0,
/*2641*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_ADD_I64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 3/*#Ops*/, 3, 4, 2, 
              // Src: (atomic_load_add:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)<<P:Predicate_atomic_load_add_64>> - Complexity = 13
              // Dst: (ATOMIC_LOAD_ADD_I64:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)
            0, // EndSwitchType
          /*SwitchOpcode*/ 79,  TARGET_OPCODE(ISD::ATOMIC_LOAD_SUB),// ->2734
/*2655*/    OPC_RecordMemRef,
/*2656*/    OPC_RecordNode,   // #0 = 'atomic_load_sub' chained node
/*2657*/    OPC_RecordChild1, // #1 = $ptr
/*2658*/    OPC_RecordChild2, // #2 = $incr
/*2659*/    OPC_SwitchType /*2 cases */, 53,  MVT::i32,// ->2715
/*2662*/      OPC_Scope, 16, /*->2680*/ // 3 children in Scope
/*2664*/        OPC_CheckPredicate, 34, // Predicate_atomic_load_sub_8
/*2666*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2669*/        OPC_EmitMergeInputChains1_0,
/*2670*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_SUB_I8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_sub:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_sub_8>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_SUB_I8:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2680*/      /*Scope*/ 16, /*->2697*/
/*2681*/        OPC_CheckPredicate, 35, // Predicate_atomic_load_sub_16
/*2683*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2686*/        OPC_EmitMergeInputChains1_0,
/*2687*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_SUB_I16), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_sub:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_sub_16>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_SUB_I16:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2697*/      /*Scope*/ 16, /*->2714*/
/*2698*/        OPC_CheckPredicate, 36, // Predicate_atomic_load_sub_32
/*2700*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2703*/        OPC_EmitMergeInputChains1_0,
/*2704*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_SUB_I32), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_sub:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_sub_32>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_SUB_I32:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2714*/      0, /*End of Scope*/
            /*SwitchType*/ 16,  MVT::i64,// ->2733
/*2717*/      OPC_CheckPredicate, 37, // Predicate_atomic_load_sub_64
/*2719*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2722*/      OPC_EmitMergeInputChains1_0,
/*2723*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_SUB_I64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 3/*#Ops*/, 3, 4, 2, 
              // Src: (atomic_load_sub:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)<<P:Predicate_atomic_load_sub_64>> - Complexity = 13
              // Dst: (ATOMIC_LOAD_SUB_I64:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)
            0, // EndSwitchType
          /*SwitchOpcode*/ 79,  TARGET_OPCODE(ISD::ATOMIC_LOAD_AND),// ->2816
/*2737*/    OPC_RecordMemRef,
/*2738*/    OPC_RecordNode,   // #0 = 'atomic_load_and' chained node
/*2739*/    OPC_RecordChild1, // #1 = $ptr
/*2740*/    OPC_RecordChild2, // #2 = $incr
/*2741*/    OPC_SwitchType /*2 cases */, 53,  MVT::i32,// ->2797
/*2744*/      OPC_Scope, 16, /*->2762*/ // 3 children in Scope
/*2746*/        OPC_CheckPredicate, 38, // Predicate_atomic_load_and_8
/*2748*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2751*/        OPC_EmitMergeInputChains1_0,
/*2752*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_AND_I8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_and:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_and_8>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_AND_I8:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2762*/      /*Scope*/ 16, /*->2779*/
/*2763*/        OPC_CheckPredicate, 39, // Predicate_atomic_load_and_16
/*2765*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2768*/        OPC_EmitMergeInputChains1_0,
/*2769*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_AND_I16), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_and:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_and_16>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_AND_I16:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2779*/      /*Scope*/ 16, /*->2796*/
/*2780*/        OPC_CheckPredicate, 40, // Predicate_atomic_load_and_32
/*2782*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2785*/        OPC_EmitMergeInputChains1_0,
/*2786*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_AND_I32), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_and:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_and_32>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_AND_I32:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2796*/      0, /*End of Scope*/
            /*SwitchType*/ 16,  MVT::i64,// ->2815
/*2799*/      OPC_CheckPredicate, 41, // Predicate_atomic_load_and_64
/*2801*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2804*/      OPC_EmitMergeInputChains1_0,
/*2805*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_AND_I64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 3/*#Ops*/, 3, 4, 2, 
              // Src: (atomic_load_and:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)<<P:Predicate_atomic_load_and_64>> - Complexity = 13
              // Dst: (ATOMIC_LOAD_AND_I64:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)
            0, // EndSwitchType
          /*SwitchOpcode*/ 79,  TARGET_OPCODE(ISD::ATOMIC_LOAD_OR),// ->2898
/*2819*/    OPC_RecordMemRef,
/*2820*/    OPC_RecordNode,   // #0 = 'atomic_load_or' chained node
/*2821*/    OPC_RecordChild1, // #1 = $ptr
/*2822*/    OPC_RecordChild2, // #2 = $incr
/*2823*/    OPC_SwitchType /*2 cases */, 53,  MVT::i32,// ->2879
/*2826*/      OPC_Scope, 16, /*->2844*/ // 3 children in Scope
/*2828*/        OPC_CheckPredicate, 42, // Predicate_atomic_load_or_8
/*2830*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2833*/        OPC_EmitMergeInputChains1_0,
/*2834*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_OR_I8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_or:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_or_8>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_OR_I8:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2844*/      /*Scope*/ 16, /*->2861*/
/*2845*/        OPC_CheckPredicate, 43, // Predicate_atomic_load_or_16
/*2847*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2850*/        OPC_EmitMergeInputChains1_0,
/*2851*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_OR_I16), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_or:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_or_16>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_OR_I16:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2861*/      /*Scope*/ 16, /*->2878*/
/*2862*/        OPC_CheckPredicate, 44, // Predicate_atomic_load_or_32
/*2864*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2867*/        OPC_EmitMergeInputChains1_0,
/*2868*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_OR_I32), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_or:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_or_32>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_OR_I32:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2878*/      0, /*End of Scope*/
            /*SwitchType*/ 16,  MVT::i64,// ->2897
/*2881*/      OPC_CheckPredicate, 45, // Predicate_atomic_load_or_64
/*2883*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2886*/      OPC_EmitMergeInputChains1_0,
/*2887*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_OR_I64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 3/*#Ops*/, 3, 4, 2, 
              // Src: (atomic_load_or:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)<<P:Predicate_atomic_load_or_64>> - Complexity = 13
              // Dst: (ATOMIC_LOAD_OR_I64:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)
            0, // EndSwitchType
          /*SwitchOpcode*/ 79,  TARGET_OPCODE(ISD::ATOMIC_LOAD_XOR),// ->2980
/*2901*/    OPC_RecordMemRef,
/*2902*/    OPC_RecordNode,   // #0 = 'atomic_load_xor' chained node
/*2903*/    OPC_RecordChild1, // #1 = $ptr
/*2904*/    OPC_RecordChild2, // #2 = $incr
/*2905*/    OPC_SwitchType /*2 cases */, 53,  MVT::i32,// ->2961
/*2908*/      OPC_Scope, 16, /*->2926*/ // 3 children in Scope
/*2910*/        OPC_CheckPredicate, 46, // Predicate_atomic_load_xor_8
/*2912*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2915*/        OPC_EmitMergeInputChains1_0,
/*2916*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_XOR_I8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_xor:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_xor_8>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_XOR_I8:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2926*/      /*Scope*/ 16, /*->2943*/
/*2927*/        OPC_CheckPredicate, 47, // Predicate_atomic_load_xor_16
/*2929*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2932*/        OPC_EmitMergeInputChains1_0,
/*2933*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_XOR_I16), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_xor:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_xor_16>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_XOR_I16:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2943*/      /*Scope*/ 16, /*->2960*/
/*2944*/        OPC_CheckPredicate, 48, // Predicate_atomic_load_xor_32
/*2946*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2949*/        OPC_EmitMergeInputChains1_0,
/*2950*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_XOR_I32), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_xor:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_xor_32>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_XOR_I32:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*2960*/      0, /*End of Scope*/
            /*SwitchType*/ 16,  MVT::i64,// ->2979
/*2963*/      OPC_CheckPredicate, 49, // Predicate_atomic_load_xor_64
/*2965*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2968*/      OPC_EmitMergeInputChains1_0,
/*2969*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_XOR_I64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 3/*#Ops*/, 3, 4, 2, 
              // Src: (atomic_load_xor:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)<<P:Predicate_atomic_load_xor_64>> - Complexity = 13
              // Dst: (ATOMIC_LOAD_XOR_I64:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)
            0, // EndSwitchType
          /*SwitchOpcode*/ 79,  TARGET_OPCODE(ISD::ATOMIC_LOAD_NAND),// ->3062
/*2983*/    OPC_RecordMemRef,
/*2984*/    OPC_RecordNode,   // #0 = 'atomic_load_nand' chained node
/*2985*/    OPC_RecordChild1, // #1 = $ptr
/*2986*/    OPC_RecordChild2, // #2 = $incr
/*2987*/    OPC_SwitchType /*2 cases */, 53,  MVT::i32,// ->3043
/*2990*/      OPC_Scope, 16, /*->3008*/ // 3 children in Scope
/*2992*/        OPC_CheckPredicate, 50, // Predicate_atomic_load_nand_8
/*2994*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*2997*/        OPC_EmitMergeInputChains1_0,
/*2998*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_NAND_I8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_nand:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_nand_8>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_NAND_I8:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*3008*/      /*Scope*/ 16, /*->3025*/
/*3009*/        OPC_CheckPredicate, 51, // Predicate_atomic_load_nand_16
/*3011*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*3014*/        OPC_EmitMergeInputChains1_0,
/*3015*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_NAND_I16), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_nand:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_nand_16>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_NAND_I16:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*3025*/      /*Scope*/ 16, /*->3042*/
/*3026*/        OPC_CheckPredicate, 52, // Predicate_atomic_load_nand_32
/*3028*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*3031*/        OPC_EmitMergeInputChains1_0,
/*3032*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_NAND_I32), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_load_nand:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)<<P:Predicate_atomic_load_nand_32>> - Complexity = 13
                // Dst: (ATOMIC_LOAD_NAND_I32:i32 xoaddr:iPTR:$ptr, GPRC:i32:$incr)
/*3042*/      0, /*End of Scope*/
            /*SwitchType*/ 16,  MVT::i64,// ->3061
/*3045*/      OPC_CheckPredicate, 53, // Predicate_atomic_load_nand_64
/*3047*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*3050*/      OPC_EmitMergeInputChains1_0,
/*3051*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_LOAD_NAND_I64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 3/*#Ops*/, 3, 4, 2, 
              // Src: (atomic_load_nand:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)<<P:Predicate_atomic_load_nand_64>> - Complexity = 13
              // Dst: (ATOMIC_LOAD_NAND_I64:i64 xoaddr:iPTR:$ptr, G8RC:i64:$incr)
            0, // EndSwitchType
          /*SwitchOpcode*/ 84,  TARGET_OPCODE(ISD::ATOMIC_CMP_SWAP),// ->3149
/*3065*/    OPC_RecordMemRef,
/*3066*/    OPC_RecordNode,   // #0 = 'atomic_cmp_swap' chained node
/*3067*/    OPC_RecordChild1, // #1 = $ptr
/*3068*/    OPC_RecordChild2, // #2 = $old
/*3069*/    OPC_RecordChild3, // #3 = $new
/*3070*/    OPC_SwitchType /*2 cases */, 56,  MVT::i32,// ->3129
/*3073*/      OPC_Scope, 17, /*->3092*/ // 3 children in Scope
/*3075*/        OPC_CheckPredicate, 54, // Predicate_atomic_cmp_swap_8
/*3077*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #4 #5
/*3080*/        OPC_EmitMergeInputChains1_0,
/*3081*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_CMP_SWAP_I8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 4/*#Ops*/, 4, 5, 2, 3, 
                // Src: (atomic_cmp_swap:i32 xoaddr:iPTR:$ptr, GPRC:i32:$old, GPRC:i32:$new)<<P:Predicate_atomic_cmp_swap_8>> - Complexity = 13
                // Dst: (ATOMIC_CMP_SWAP_I8:i32 xoaddr:iPTR:$ptr, GPRC:i32:$old, GPRC:i32:$new)
/*3092*/      /*Scope*/ 17, /*->3110*/
/*3093*/        OPC_CheckPredicate, 55, // Predicate_atomic_cmp_swap_16
/*3095*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #4 #5
/*3098*/        OPC_EmitMergeInputChains1_0,
/*3099*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_CMP_SWAP_I16), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 4/*#Ops*/, 4, 5, 2, 3, 
                // Src: (atomic_cmp_swap:i32 xoaddr:iPTR:$ptr, GPRC:i32:$old, GPRC:i32:$new)<<P:Predicate_atomic_cmp_swap_16>> - Complexity = 13
                // Dst: (ATOMIC_CMP_SWAP_I16:i32 xoaddr:iPTR:$ptr, GPRC:i32:$old, GPRC:i32:$new)
/*3110*/      /*Scope*/ 17, /*->3128*/
/*3111*/        OPC_CheckPredicate, 56, // Predicate_atomic_cmp_swap_32
/*3113*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #4 #5
/*3116*/        OPC_EmitMergeInputChains1_0,
/*3117*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_CMP_SWAP_I32), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 4/*#Ops*/, 4, 5, 2, 3, 
                // Src: (atomic_cmp_swap:i32 xoaddr:iPTR:$ptr, GPRC:i32:$old, GPRC:i32:$new)<<P:Predicate_atomic_cmp_swap_32>> - Complexity = 13
                // Dst: (ATOMIC_CMP_SWAP_I32:i32 xoaddr:iPTR:$ptr, GPRC:i32:$old, GPRC:i32:$new)
/*3128*/      0, /*End of Scope*/
            /*SwitchType*/ 17,  MVT::i64,// ->3148
/*3131*/      OPC_CheckPredicate, 57, // Predicate_atomic_cmp_swap_64
/*3133*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #4 #5
/*3136*/      OPC_EmitMergeInputChains1_0,
/*3137*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_CMP_SWAP_I64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 4/*#Ops*/, 4, 5, 2, 3, 
              // Src: (atomic_cmp_swap:i64 xoaddr:iPTR:$ptr, G8RC:i64:$old, G8RC:i64:$new)<<P:Predicate_atomic_cmp_swap_64>> - Complexity = 13
              // Dst: (ATOMIC_CMP_SWAP_I64:i64 xoaddr:iPTR:$ptr, G8RC:i64:$old, G8RC:i64:$new)
            0, // EndSwitchType
          /*SwitchOpcode*/ 79,  TARGET_OPCODE(ISD::ATOMIC_SWAP),// ->3231
/*3152*/    OPC_RecordMemRef,
/*3153*/    OPC_RecordNode,   // #0 = 'atomic_swap' chained node
/*3154*/    OPC_RecordChild1, // #1 = $ptr
/*3155*/    OPC_RecordChild2, // #2 = $new
/*3156*/    OPC_SwitchType /*2 cases */, 53,  MVT::i32,// ->3212
/*3159*/      OPC_Scope, 16, /*->3177*/ // 3 children in Scope
/*3161*/        OPC_CheckPredicate, 58, // Predicate_atomic_swap_8
/*3163*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*3166*/        OPC_EmitMergeInputChains1_0,
/*3167*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_SWAP_I8), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_swap:i32 xoaddr:iPTR:$ptr, GPRC:i32:$new)<<P:Predicate_atomic_swap_8>> - Complexity = 13
                // Dst: (ATOMIC_SWAP_I8:i32 xoaddr:iPTR:$ptr, GPRC:i32:$new)
/*3177*/      /*Scope*/ 16, /*->3194*/
/*3178*/        OPC_CheckPredicate, 59, // Predicate_atomic_swap_16
/*3180*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*3183*/        OPC_EmitMergeInputChains1_0,
/*3184*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_SWAP_I16), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_swap:i32 xoaddr:iPTR:$ptr, GPRC:i32:$new)<<P:Predicate_atomic_swap_16>> - Complexity = 13
                // Dst: (ATOMIC_SWAP_I16:i32 xoaddr:iPTR:$ptr, GPRC:i32:$new)
/*3194*/      /*Scope*/ 16, /*->3211*/
/*3195*/        OPC_CheckPredicate, 60, // Predicate_atomic_swap_32
/*3197*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*3200*/        OPC_EmitMergeInputChains1_0,
/*3201*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_SWAP_I32), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 4, 2, 
                // Src: (atomic_swap:i32 xoaddr:iPTR:$ptr, GPRC:i32:$new)<<P:Predicate_atomic_swap_32>> - Complexity = 13
                // Dst: (ATOMIC_SWAP_I32:i32 xoaddr:iPTR:$ptr, GPRC:i32:$new)
/*3211*/      0, /*End of Scope*/
            /*SwitchType*/ 16,  MVT::i64,// ->3230
/*3214*/      OPC_CheckPredicate, 61, // Predicate_atomic_swap_64
/*3216*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #3 #4
/*3219*/      OPC_EmitMergeInputChains1_0,
/*3220*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::ATOMIC_SWAP_I64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 3/*#Ops*/, 3, 4, 2, 
              // Src: (atomic_swap:i64 xoaddr:iPTR:$ptr, G8RC:i64:$new)<<P:Predicate_atomic_swap_64>> - Complexity = 13
              // Dst: (ATOMIC_SWAP_I64:i64 xoaddr:iPTR:$ptr, G8RC:i64:$new)
            0, // EndSwitchType
          /*SwitchOpcode*/ 48,  TARGET_OPCODE(PPCISD::DYNALLOC),// ->3282
/*3234*/    OPC_RecordNode,   // #0 = 'PPCdynalloc' chained node
/*3235*/    OPC_RecordChild1, // #1 = $negsize
/*3236*/    OPC_SwitchType /*2 cases */, 20,  MVT::i32,// ->3259
/*3239*/      OPC_CheckChild1Type, MVT::i32,
/*3241*/      OPC_RecordChild2, // #2 = $fpsi
/*3242*/      OPC_CheckChild2Type, MVT::iPTR,
/*3244*/      OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectAddrImm:$fpsi #3 #4
/*3247*/      OPC_EmitMergeInputChains1_0,
/*3248*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::DYNALLOC), 0|OPFL_Chain,
                  2/*#VTs*/, MVT::i32, MVT::i32, 3/*#Ops*/, 1, 3, 4, 
              // Src: (PPCdynalloc:i32 GPRC:i32:$negsize, iaddr:iPTR:$fpsi) - Complexity = 12
              // Dst: (DYNALLOC:i32:i32 GPRC:i32:$negsize, iaddr:iPTR:$fpsi)
            /*SwitchType*/ 20,  MVT::i64,// ->3281
/*3261*/      OPC_CheckChild1Type, MVT::i64,
/*3263*/      OPC_RecordChild2, // #2 = $fpsi
/*3264*/      OPC_CheckChild2Type, MVT::iPTR,
/*3266*/      OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectAddrImm:$fpsi #3 #4
/*3269*/      OPC_EmitMergeInputChains1_0,
/*3270*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::DYNALLOC8), 0|OPFL_Chain,
                  2/*#VTs*/, MVT::i64, MVT::i64, 3/*#Ops*/, 1, 3, 4, 
              // Src: (PPCdynalloc:i64 G8RC:i64:$negsize, iaddr:iPTR:$fpsi) - Complexity = 12
              // Dst: (DYNALLOC8:i64:i64 G8RC:i64:$negsize, iaddr:iPTR:$fpsi)
            0, // EndSwitchType
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(PPCISD::LARX),// ->3319
/*3285*/    OPC_RecordNode,   // #0 = 'PPClarx' chained node
/*3286*/    OPC_RecordChild1, // #1 = $src
/*3287*/    OPC_SwitchType /*2 cases */, 13,  MVT::i32,// ->3303
/*3290*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$src #2 #3
/*3293*/      OPC_EmitMergeInputChains1_0,
/*3294*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LWARX), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (PPClarx:i32 xoaddr:iPTR:$src) - Complexity = 12
              // Dst: (LWARX:i32 xoaddr:iPTR:$src)
            /*SwitchType*/ 13,  MVT::i64,// ->3318
/*3305*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$ptr #2 #3
/*3308*/      OPC_EmitMergeInputChains1_0,
/*3309*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LDARX), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
              // Src: (PPClarx:i64 xoaddr:iPTR:$ptr) - Complexity = 12
              // Dst: (LDARX:i64 xoaddr:iPTR:$ptr)
            0, // EndSwitchType
          /*SwitchOpcode*/ 40,  TARGET_OPCODE(PPCISD::LBRX),// ->3362
/*3322*/    OPC_RecordNode,   // #0 = 'PPClbrx' chained node
/*3323*/    OPC_RecordChild1, // #1 = $src
/*3324*/    OPC_MoveChild, 2,
/*3326*/    OPC_Scope, 16, /*->3344*/ // 2 children in Scope
/*3328*/      OPC_CheckValueType, MVT::i16,
/*3330*/      OPC_MoveParent,
/*3331*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$src #2 #3
/*3334*/      OPC_EmitMergeInputChains1_0,
/*3335*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LHBRX), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (PPClbrx:i32 xoaddr:iPTR:$src, i16:Other) - Complexity = 12
              // Dst: (LHBRX:i32 xoaddr:iPTR:$src)
/*3344*/    /*Scope*/ 16, /*->3361*/
/*3345*/      OPC_CheckValueType, MVT::i32,
/*3347*/      OPC_MoveParent,
/*3348*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$src #2 #3
/*3351*/      OPC_EmitMergeInputChains1_0,
/*3352*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LWBRX), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (PPClbrx:i32 xoaddr:iPTR:$src, i32:Other) - Complexity = 12
              // Dst: (LWBRX:i32 xoaddr:iPTR:$src)
/*3361*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 37,  TARGET_OPCODE(PPCISD::LOAD),// ->3402
/*3365*/    OPC_RecordNode,   // #0 = 'PPCload' chained node
/*3366*/    OPC_CaptureFlagInput,
/*3367*/    OPC_RecordChild1, // #1 = $src
/*3368*/    OPC_CheckChild1Type, MVT::iPTR,
/*3370*/    OPC_CheckType, MVT::i64,
/*3372*/    OPC_Scope, 13, /*->3387*/ // 2 children in Scope
/*3374*/      OPC_CheckComplexPat, /*CP*/3, /*#*/1, // SelectAddrImmShift:$src #2 #3
/*3377*/      OPC_EmitMergeInputChains1_0,
/*3378*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LD), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
              // Src: (PPCload:i64 ixaddr:iPTR:$src) - Complexity = 12
              // Dst: (LD:i64 ixaddr:iPTR:$src)
/*3387*/    /*Scope*/ 13, /*->3401*/
/*3388*/      OPC_CheckComplexPat, /*CP*/2, /*#*/1, // SelectAddrIdx:$src #2 #3
/*3391*/      OPC_EmitMergeInputChains1_0,
/*3392*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LDX), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
              // Src: (PPCload:i64 xaddr:iPTR:$src) - Complexity = 12
              // Dst: (LDX:i64 xaddr:iPTR:$src)
/*3401*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 84|128,4/*596*/,  TARGET_OPCODE(ISD::XOR),// ->4002
/*3406*/    OPC_Scope, 39|128,1/*167*/, /*->3576*/ // 8 children in Scope
/*3409*/      OPC_MoveChild, 0,
/*3411*/      OPC_SwitchOpcode /*3 cases */, 41,  TARGET_OPCODE(ISD::AND),// ->3456
/*3415*/        OPC_RecordChild0, // #0 = $rS
/*3416*/        OPC_RecordChild1, // #1 = $rB
/*3417*/        OPC_MoveParent,
/*3418*/        OPC_MoveChild, 1,
/*3420*/        OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3431*/        OPC_MoveParent,
/*3432*/        OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->3444
/*3435*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::NAND), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (xor:i32 (and:i32 GPRC:i32:$rS, GPRC:i32:$rB), -1:i32) - Complexity = 11
                  // Dst: (NAND:i32 GPRC:i32:$rS, GPRC:i32:$rB)
                /*SwitchType*/ 9,  MVT::i64,// ->3455
/*3446*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::NAND8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (xor:i64 (and:i64 G8RC:i64:$rS, G8RC:i64:$rB), -1:i64) - Complexity = 11
                  // Dst: (NAND8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
                0, // EndSwitchType
              /*SwitchOpcode*/ 41,  TARGET_OPCODE(ISD::OR),// ->3500
/*3459*/        OPC_RecordChild0, // #0 = $rS
/*3460*/        OPC_RecordChild1, // #1 = $rB
/*3461*/        OPC_MoveParent,
/*3462*/        OPC_MoveChild, 1,
/*3464*/        OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3475*/        OPC_MoveParent,
/*3476*/        OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->3488
/*3479*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::NOR), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (xor:i32 (or:i32 GPRC:i32:$rS, GPRC:i32:$rB), -1:i32) - Complexity = 11
                  // Dst: (NOR:i32 GPRC:i32:$rS, GPRC:i32:$rB)
                /*SwitchType*/ 9,  MVT::i64,// ->3499
/*3490*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::NOR8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (xor:i64 (or:i64 G8RC:i64:$rS, G8RC:i64:$rB), -1:i64) - Complexity = 11
                  // Dst: (NOR8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
                0, // EndSwitchType
              /*SwitchOpcode*/ 72,  TARGET_OPCODE(ISD::XOR),// ->3575
/*3503*/        OPC_RecordChild0, // #0 = $rS
/*3504*/        OPC_Scope, 40, /*->3546*/ // 2 children in Scope
/*3506*/          OPC_RecordChild1, // #1 = $rB
/*3507*/          OPC_MoveParent,
/*3508*/          OPC_MoveChild, 1,
/*3510*/          OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3521*/          OPC_MoveParent,
/*3522*/          OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->3534
/*3525*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::EQV), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                    // Src: (xor:i32 (xor:i32 GPRC:i32:$rS, GPRC:i32:$rB), -1:i32) - Complexity = 11
                    // Dst: (EQV:i32 GPRC:i32:$rS, GPRC:i32:$rB)
                  /*SwitchType*/ 9,  MVT::i64,// ->3545
/*3536*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::EQV8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                    // Src: (xor:i64 (xor:i64 G8RC:i64:$rS, G8RC:i64:$rB), -1:i64) - Complexity = 11
                    // Dst: (EQV8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
                  0, // EndSwitchType
/*3546*/        /*Scope*/ 27, /*->3574*/
/*3547*/          OPC_MoveChild, 1,
/*3549*/          OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3560*/          OPC_MoveParent,
/*3561*/          OPC_MoveParent,
/*3562*/          OPC_RecordChild1, // #1 = $rB
/*3563*/          OPC_CheckType, MVT::i32,
/*3565*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::EQV), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (xor:i32 (xor:i32 GPRC:i32:$rS, -1:i32), GPRC:i32:$rB) - Complexity = 11
                  // Dst: (EQV:i32 GPRC:i32:$rS, GPRC:i32:$rB)
/*3574*/        0, /*End of Scope*/
              0, // EndSwitchOpcode
/*3576*/    /*Scope*/ 33, /*->3610*/
/*3577*/      OPC_RecordChild0, // #0 = $rB
/*3578*/      OPC_MoveChild, 1,
/*3580*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*3583*/      OPC_RecordChild0, // #1 = $rS
/*3584*/      OPC_MoveChild, 1,
/*3586*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3597*/      OPC_MoveParent,
/*3598*/      OPC_MoveParent,
/*3599*/      OPC_CheckType, MVT::i32,
/*3601*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::EQV), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
              // Src: (xor:i32 GPRC:i32:$rB, (xor:i32 GPRC:i32:$rS, -1:i32)) - Complexity = 11
              // Dst: (EQV:i32 GPRC:i32:$rS, GPRC:i32:$rB)
/*3610*/    /*Scope*/ 33, /*->3644*/
/*3611*/      OPC_MoveChild, 0,
/*3613*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*3616*/      OPC_RecordChild0, // #0 = $rS
/*3617*/      OPC_MoveChild, 1,
/*3619*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3630*/      OPC_MoveParent,
/*3631*/      OPC_MoveParent,
/*3632*/      OPC_RecordChild1, // #1 = $rB
/*3633*/      OPC_CheckType, MVT::i64,
/*3635*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::EQV8), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (xor:i64 (xor:i64 G8RC:i64:$rS, -1:i64), G8RC:i64:$rB) - Complexity = 11
              // Dst: (EQV8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
/*3644*/    /*Scope*/ 84|128,1/*212*/, /*->3858*/
/*3646*/      OPC_RecordChild0, // #0 = $rB
/*3647*/      OPC_Scope, 59, /*->3708*/ // 2 children in Scope
/*3649*/        OPC_MoveChild, 1,
/*3651*/        OPC_Scope, 30, /*->3683*/ // 2 children in Scope
/*3653*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*3656*/          OPC_RecordChild0, // #1 = $rS
/*3657*/          OPC_MoveChild, 1,
/*3659*/          OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3670*/          OPC_MoveParent,
/*3671*/          OPC_MoveParent,
/*3672*/          OPC_CheckType, MVT::i64,
/*3674*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::EQV8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                  // Src: (xor:i64 G8RC:i64:$rB, (xor:i64 G8RC:i64:$rS, -1:i64)) - Complexity = 11
                  // Dst: (EQV8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
/*3683*/        /*Scope*/ 23, /*->3707*/
/*3684*/          OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3695*/          OPC_MoveParent,
/*3696*/          OPC_CheckType, MVT::i32,
/*3698*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::NOR), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 0, 
                  // Src: (xor:i32 GPRC:i32:$in, -1:i32) - Complexity = 8
                  // Dst: (NOR:i32 GPRC:i32:$in, GPRC:i32:$in)
/*3707*/        0, /*End of Scope*/
/*3708*/      /*Scope*/ 19|128,1/*147*/, /*->3857*/
/*3710*/        OPC_RecordChild1, // #1 = $src2
/*3711*/        OPC_Scope, 119, /*->3832*/ // 3 children in Scope
/*3713*/          OPC_MoveChild, 1,
/*3715*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*3718*/          OPC_Scope, 19, /*->3739*/ // 5 children in Scope
/*3720*/            OPC_CheckPredicate, 62, // Predicate_immZExt16
/*3722*/            OPC_MoveParent,
/*3723*/            OPC_CheckType, MVT::i32,
/*3725*/            OPC_EmitConvertToTarget, 1,
/*3727*/            OPC_EmitNodeXForm, 1, 2, // LO16
/*3730*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::XORI), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                    // Src: (xor:i32 GPRC:i32:$src1, (imm:i32)<<P:Predicate_immZExt16>><<X:LO16>>:$src2) - Complexity = 7
                    // Dst: (XORI:i32 GPRC:i32:$src1, (LO16:i32 (imm:i32):$src2))
/*3739*/          /*Scope*/ 19, /*->3759*/
/*3740*/            OPC_CheckPredicate, 63, // Predicate_imm16ShiftedZExt
/*3742*/            OPC_MoveParent,
/*3743*/            OPC_CheckType, MVT::i32,
/*3745*/            OPC_EmitConvertToTarget, 1,
/*3747*/            OPC_EmitNodeXForm, 0, 2, // HI16
/*3750*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::XORIS), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                    // Src: (xor:i32 GPRC:i32:$src1, (imm:i32)<<P:Predicate_imm16ShiftedZExt>><<X:HI16>>:$src2) - Complexity = 7
                    // Dst: (XORIS:i32 GPRC:i32:$src1, (HI16:i32 (imm:i32):$src2))
/*3759*/          /*Scope*/ 19, /*->3779*/
/*3760*/            OPC_CheckPredicate, 62, // Predicate_immZExt16
/*3762*/            OPC_MoveParent,
/*3763*/            OPC_CheckType, MVT::i64,
/*3765*/            OPC_EmitConvertToTarget, 1,
/*3767*/            OPC_EmitNodeXForm, 1, 2, // LO16
/*3770*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::XORI8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                    // Src: (xor:i64 G8RC:i64:$src1, (imm:i64)<<P:Predicate_immZExt16>><<X:LO16>>:$src2) - Complexity = 7
                    // Dst: (XORI8:i64 G8RC:i64:$src1, (LO16:i32 (imm:i64):$src2))
/*3779*/          /*Scope*/ 19, /*->3799*/
/*3780*/            OPC_CheckPredicate, 63, // Predicate_imm16ShiftedZExt
/*3782*/            OPC_MoveParent,
/*3783*/            OPC_CheckType, MVT::i64,
/*3785*/            OPC_EmitConvertToTarget, 1,
/*3787*/            OPC_EmitNodeXForm, 0, 2, // HI16
/*3790*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::XORIS8), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                    // Src: (xor:i64 G8RC:i64:$src1, (imm:i64)<<P:Predicate_imm16ShiftedZExt>><<X:HI16>>:$src2) - Complexity = 7
                    // Dst: (XORIS8:i64 G8RC:i64:$src1, (HI16:i32 (imm:i64):$src2))
/*3799*/          /*Scope*/ 31, /*->3831*/
/*3800*/            OPC_MoveParent,
/*3801*/            OPC_CheckType, MVT::i32,
/*3803*/            OPC_EmitConvertToTarget, 1,
/*3805*/            OPC_EmitNodeXForm, 1, 2, // LO16
/*3808*/            OPC_EmitNode, TARGET_OPCODE(PPC::XORI), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3,  // Results = #4 
/*3817*/            OPC_EmitConvertToTarget, 1,
/*3819*/            OPC_EmitNodeXForm, 0, 5, // HI16
/*3822*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::XORIS), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 4, 6, 
                    // Src: (xor:i32 GPRC:i32:$in, (imm:i32):$imm) - Complexity = 6
                    // Dst: (XORIS:i32 (XORI:i32 GPRC:i32:$in, (LO16:i32 (imm:i32):$imm)), (HI16:i32 (imm:i32):$imm))
/*3831*/          0, /*End of Scope*/
/*3832*/        /*Scope*/ 11, /*->3844*/
/*3833*/          OPC_CheckType, MVT::i32,
/*3835*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::XOR), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (xor:i32 GPRC:i32:$rS, GPRC:i32:$rB) - Complexity = 3
                  // Dst: (XOR:i32 GPRC:i32:$rS, GPRC:i32:$rB)
/*3844*/        /*Scope*/ 11, /*->3856*/
/*3845*/          OPC_CheckType, MVT::i64,
/*3847*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::XOR8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (xor:i64 G8RC:i64:$rS, G8RC:i64:$rB) - Complexity = 3
                  // Dst: (XOR8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
/*3856*/        0, /*End of Scope*/
/*3857*/      0, /*End of Scope*/
/*3858*/    /*Scope*/ 70, /*->3929*/
/*3859*/      OPC_MoveChild, 0,
/*3861*/      OPC_SwitchOpcode /*2 cases */, 30,  TARGET_OPCODE(ISD::OR),// ->3895
/*3865*/        OPC_RecordChild0, // #0 = $vA
/*3866*/        OPC_RecordChild1, // #1 = $vB
/*3867*/        OPC_MoveParent,
/*3868*/        OPC_MoveChild, 1,
/*3870*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BIT_CONVERT),
/*3873*/        OPC_MoveChild, 0,
/*3875*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*3878*/        OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*3880*/        OPC_CheckType, MVT::v16i8,
/*3882*/        OPC_MoveParent,
/*3883*/        OPC_MoveParent,
/*3884*/        OPC_CheckType, MVT::v4i32,
/*3886*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VNOR), 0,
                    1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
                // Src: (xor:v4i32 (or:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB), (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>)) - Complexity = 13
                // Dst: (VNOR:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
              /*SwitchOpcode*/ 30,  TARGET_OPCODE(ISD::BIT_CONVERT),// ->3928
/*3898*/        OPC_MoveChild, 0,
/*3900*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*3903*/        OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*3905*/        OPC_CheckType, MVT::v16i8,
/*3907*/        OPC_MoveParent,
/*3908*/        OPC_MoveParent,
/*3909*/        OPC_MoveChild, 1,
/*3911*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::OR),
/*3914*/        OPC_RecordChild0, // #0 = $vA
/*3915*/        OPC_RecordChild1, // #1 = $vB
/*3916*/        OPC_MoveParent,
/*3917*/        OPC_CheckType, MVT::v4i32,
/*3919*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VNOR), 0,
                    1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
                // Src: (xor:v4i32 (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>), (or:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)) - Complexity = 13
                // Dst: (VNOR:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
              0, // EndSwitchOpcode
/*3929*/    /*Scope*/ 28, /*->3958*/
/*3930*/      OPC_RecordChild0, // #0 = $vA
/*3931*/      OPC_MoveChild, 1,
/*3933*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BIT_CONVERT),
/*3936*/      OPC_MoveChild, 0,
/*3938*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*3941*/      OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*3943*/      OPC_CheckType, MVT::v16i8,
/*3945*/      OPC_MoveParent,
/*3946*/      OPC_MoveParent,
/*3947*/      OPC_CheckType, MVT::v4i32,
/*3949*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VNOR), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 0, 
              // Src: (xor:v4i32 VRRC:v4i32:$vA, (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>)) - Complexity = 10
              // Dst: (VNOR:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vA)
/*3958*/    /*Scope*/ 28, /*->3987*/
/*3959*/      OPC_MoveChild, 0,
/*3961*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BIT_CONVERT),
/*3964*/      OPC_MoveChild, 0,
/*3966*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*3969*/      OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*3971*/      OPC_CheckType, MVT::v16i8,
/*3973*/      OPC_MoveParent,
/*3974*/      OPC_MoveParent,
/*3975*/      OPC_RecordChild1, // #0 = $vA
/*3976*/      OPC_CheckType, MVT::v4i32,
/*3978*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VNOR), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 0, 
              // Src: (xor:v4i32 (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>), VRRC:v4i32:$vA) - Complexity = 10
              // Dst: (VNOR:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vA)
/*3987*/    /*Scope*/ 13, /*->4001*/
/*3988*/      OPC_RecordChild0, // #0 = $vA
/*3989*/      OPC_RecordChild1, // #1 = $vB
/*3990*/      OPC_CheckType, MVT::v4i32,
/*3992*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VXOR), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (xor:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 3
              // Dst: (VXOR:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*4001*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 9|128,4/*521*/,  TARGET_OPCODE(ISD::AND),// ->4527
/*4006*/    OPC_Scope, 46, /*->4054*/ // 7 children in Scope
/*4008*/      OPC_RecordChild0, // #0 = $rS
/*4009*/      OPC_MoveChild, 1,
/*4011*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*4014*/      OPC_RecordChild0, // #1 = $rB
/*4015*/      OPC_MoveChild, 1,
/*4017*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*4028*/      OPC_MoveParent,
/*4029*/      OPC_MoveParent,
/*4030*/      OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->4042
/*4033*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ANDC), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (and:i32 GPRC:i32:$rS, (xor:i32 GPRC:i32:$rB, -1:i32)) - Complexity = 11
                // Dst: (ANDC:i32 GPRC:i32:$rS, GPRC:i32:$rB)
              /*SwitchType*/ 9,  MVT::i64,// ->4053
/*4044*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ANDC8), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (and:i64 G8RC:i64:$rS, (xor:i64 G8RC:i64:$rB, -1:i64)) - Complexity = 11
                // Dst: (ANDC8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
              0, // EndSwitchType
/*4054*/    /*Scope*/ 88, /*->4143*/
/*4055*/      OPC_MoveChild, 0,
/*4057*/      OPC_SwitchOpcode /*2 cases */, 41,  TARGET_OPCODE(ISD::XOR),// ->4102
/*4061*/        OPC_RecordChild0, // #0 = $rB
/*4062*/        OPC_MoveChild, 1,
/*4064*/        OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*4075*/        OPC_MoveParent,
/*4076*/        OPC_MoveParent,
/*4077*/        OPC_RecordChild1, // #1 = $rS
/*4078*/        OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->4090
/*4081*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ANDC), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                  // Src: (and:i32 (xor:i32 GPRC:i32:$rB, -1:i32), GPRC:i32:$rS) - Complexity = 11
                  // Dst: (ANDC:i32 GPRC:i32:$rS, GPRC:i32:$rB)
                /*SwitchType*/ 9,  MVT::i64,// ->4101
/*4092*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ANDC8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                  // Src: (and:i64 (xor:i64 G8RC:i64:$rB, -1:i64), G8RC:i64:$rS) - Complexity = 11
                  // Dst: (ANDC8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
                0, // EndSwitchType
              /*SwitchOpcode*/ 37,  TARGET_OPCODE(ISD::ROTL),// ->4142
/*4105*/        OPC_RecordChild0, // #0 = $in
/*4106*/        OPC_RecordChild1, // #1 = $sh
/*4107*/        OPC_CheckChild1Type, MVT::i32,
/*4109*/        OPC_MoveParent,
/*4110*/        OPC_RecordChild1, // #2 = $imm
/*4111*/        OPC_MoveChild, 1,
/*4113*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*4116*/        OPC_CheckPredicate, 65, // Predicate_maskimm32
/*4118*/        OPC_MoveParent,
/*4119*/        OPC_CheckType, MVT::i32,
/*4121*/        OPC_EmitConvertToTarget, 2,
/*4123*/        OPC_EmitNodeXForm, 3, 3, // MB
/*4126*/        OPC_EmitConvertToTarget, 2,
/*4128*/        OPC_EmitNodeXForm, 4, 5, // ME
/*4131*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::RLWNM), 0,
                    1/*#VTs*/, MVT::i32, 4/*#Ops*/, 0, 1, 4, 6, 
                // Src: (and:i32 (rotl:i32 GPRC:i32:$in, GPRC:i32:$sh), (imm:i32)<<P:Predicate_maskimm32>>:$imm) - Complexity = 10
                // Dst: (RLWNM:i32 GPRC:i32:$in, GPRC:i32:$sh, (MB:i32 (imm:i32)<<P:Predicate_maskimm32>>:$imm), (ME:i32 (imm:i32)<<P:Predicate_maskimm32>>:$imm))
              0, // EndSwitchOpcode
/*4143*/    /*Scope*/ 63|128,1/*191*/, /*->4336*/
/*4145*/      OPC_RecordChild0, // #0 = $src1
/*4146*/      OPC_Scope, 119, /*->4267*/ // 2 children in Scope
/*4148*/        OPC_RecordChild1, // #1 = $src2
/*4149*/        OPC_Scope, 91, /*->4242*/ // 3 children in Scope
/*4151*/          OPC_MoveChild, 1,
/*4153*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*4156*/          OPC_Scope, 20, /*->4178*/ // 4 children in Scope
/*4158*/            OPC_CheckPredicate, 62, // Predicate_immZExt16
/*4160*/            OPC_MoveParent,
/*4161*/            OPC_CheckType, MVT::i32,
/*4163*/            OPC_EmitConvertToTarget, 1,
/*4165*/            OPC_EmitNodeXForm, 1, 2, // LO16
/*4168*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::ANDIo), 0,
                        2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 0, 3, 
                    // Src: (and:i32 GPRC:i32:$src1, (imm:i32)<<P:Predicate_immZExt16>><<X:LO16>>:$src2) - Complexity = 7
                    // Dst: (ANDIo:i32:i32 GPRC:i32:$src1, (LO16:i32 (imm:i32):$src2))
/*4178*/          /*Scope*/ 20, /*->4199*/
/*4179*/            OPC_CheckPredicate, 63, // Predicate_imm16ShiftedZExt
/*4181*/            OPC_MoveParent,
/*4182*/            OPC_CheckType, MVT::i32,
/*4184*/            OPC_EmitConvertToTarget, 1,
/*4186*/            OPC_EmitNodeXForm, 0, 2, // HI16
/*4189*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::ANDISo), 0,
                        2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 0, 3, 
                    // Src: (and:i32 GPRC:i32:$src1, (imm:i32)<<P:Predicate_imm16ShiftedZExt>><<X:HI16>>:$src2) - Complexity = 7
                    // Dst: (ANDISo:i32:i32 GPRC:i32:$src1, (HI16:i32 (imm:i32):$src2))
/*4199*/          /*Scope*/ 20, /*->4220*/
/*4200*/            OPC_CheckPredicate, 62, // Predicate_immZExt16
/*4202*/            OPC_MoveParent,
/*4203*/            OPC_CheckType, MVT::i64,
/*4205*/            OPC_EmitConvertToTarget, 1,
/*4207*/            OPC_EmitNodeXForm, 1, 2, // LO16
/*4210*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::ANDIo8), 0,
                        2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 0, 3, 
                    // Src: (and:i64 G8RC:i64:$src1, (imm:i64)<<P:Predicate_immZExt16>><<X:LO16>>:$src2) - Complexity = 7
                    // Dst: (ANDIo8:i64:i32 G8RC:i64:$src1, (LO16:i32 (imm:i64):$src2))
/*4220*/          /*Scope*/ 20, /*->4241*/
/*4221*/            OPC_CheckPredicate, 63, // Predicate_imm16ShiftedZExt
/*4223*/            OPC_MoveParent,
/*4224*/            OPC_CheckType, MVT::i64,
/*4226*/            OPC_EmitConvertToTarget, 1,
/*4228*/            OPC_EmitNodeXForm, 0, 2, // HI16
/*4231*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::ANDISo8), 0,
                        2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 0, 3, 
                    // Src: (and:i64 G8RC:i64:$src1, (imm:i64)<<P:Predicate_imm16ShiftedZExt>><<X:HI16>>:$src2) - Complexity = 7
                    // Dst: (ANDISo8:i64:i32 G8RC:i64:$src1, (HI16:i32 (imm:i64):$src2))
/*4241*/          0, /*End of Scope*/
/*4242*/        /*Scope*/ 11, /*->4254*/
/*4243*/          OPC_CheckType, MVT::i32,
/*4245*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::AND), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (and:i32 GPRC:i32:$rS, GPRC:i32:$rB) - Complexity = 3
                  // Dst: (AND:i32 GPRC:i32:$rS, GPRC:i32:$rB)
/*4254*/        /*Scope*/ 11, /*->4266*/
/*4255*/          OPC_CheckType, MVT::i64,
/*4257*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::AND8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (and:i64 G8RC:i64:$rS, G8RC:i64:$rB) - Complexity = 3
                  // Dst: (AND8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
/*4266*/        0, /*End of Scope*/
/*4267*/      /*Scope*/ 67, /*->4335*/
/*4268*/        OPC_MoveChild, 1,
/*4270*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*4273*/        OPC_Scope, 29, /*->4304*/ // 2 children in Scope
/*4275*/          OPC_RecordChild0, // #1 = $vB
/*4276*/          OPC_MoveChild, 1,
/*4278*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BIT_CONVERT),
/*4281*/          OPC_MoveChild, 0,
/*4283*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*4286*/          OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*4288*/          OPC_CheckType, MVT::v16i8,
/*4290*/          OPC_MoveParent,
/*4291*/          OPC_MoveParent,
/*4292*/          OPC_MoveParent,
/*4293*/          OPC_CheckType, MVT::v4i32,
/*4295*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::VANDC), 0,
                      1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
                  // Src: (and:v4i32 VRRC:v4i32:$vA, (xor:v4i32 VRRC:v4i32:$vB, (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>))) - Complexity = 13
                  // Dst: (VANDC:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*4304*/        /*Scope*/ 29, /*->4334*/
/*4305*/          OPC_MoveChild, 0,
/*4307*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BIT_CONVERT),
/*4310*/          OPC_MoveChild, 0,
/*4312*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*4315*/          OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*4317*/          OPC_CheckType, MVT::v16i8,
/*4319*/          OPC_MoveParent,
/*4320*/          OPC_MoveParent,
/*4321*/          OPC_RecordChild1, // #1 = $vB
/*4322*/          OPC_MoveParent,
/*4323*/          OPC_CheckType, MVT::v4i32,
/*4325*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::VANDC), 0,
                      1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
                  // Src: (and:v4i32 VRRC:v4i32:$vA, (xor:v4i32 (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>), VRRC:v4i32:$vB)) - Complexity = 13
                  // Dst: (VANDC:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*4334*/        0, /*End of Scope*/
/*4335*/      0, /*End of Scope*/
/*4336*/    /*Scope*/ 69, /*->4406*/
/*4337*/      OPC_MoveChild, 0,
/*4339*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*4342*/      OPC_Scope, 30, /*->4374*/ // 2 children in Scope
/*4344*/        OPC_RecordChild0, // #0 = $vB
/*4345*/        OPC_MoveChild, 1,
/*4347*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BIT_CONVERT),
/*4350*/        OPC_MoveChild, 0,
/*4352*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*4355*/        OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*4357*/        OPC_CheckType, MVT::v16i8,
/*4359*/        OPC_MoveParent,
/*4360*/        OPC_MoveParent,
/*4361*/        OPC_MoveParent,
/*4362*/        OPC_RecordChild1, // #1 = $vA
/*4363*/        OPC_CheckType, MVT::v4i32,
/*4365*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VANDC), 0,
                    1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 1, 0, 
                // Src: (and:v4i32 (xor:v4i32 VRRC:v4i32:$vB, (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>)), VRRC:v4i32:$vA) - Complexity = 13
                // Dst: (VANDC:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*4374*/      /*Scope*/ 30, /*->4405*/
/*4375*/        OPC_MoveChild, 0,
/*4377*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BIT_CONVERT),
/*4380*/        OPC_MoveChild, 0,
/*4382*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*4385*/        OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*4387*/        OPC_CheckType, MVT::v16i8,
/*4389*/        OPC_MoveParent,
/*4390*/        OPC_MoveParent,
/*4391*/        OPC_RecordChild1, // #0 = $vB
/*4392*/        OPC_MoveParent,
/*4393*/        OPC_RecordChild1, // #1 = $vA
/*4394*/        OPC_CheckType, MVT::v4i32,
/*4396*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VANDC), 0,
                    1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 1, 0, 
                // Src: (and:v4i32 (xor:v4i32 (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>), VRRC:v4i32:$vB), VRRC:v4i32:$vA) - Complexity = 13
                // Dst: (VANDC:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*4405*/      0, /*End of Scope*/
/*4406*/    /*Scope*/ 35, /*->4442*/
/*4407*/      OPC_RecordChild0, // #0 = $A
/*4408*/      OPC_MoveChild, 1,
/*4410*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*4413*/      OPC_MoveChild, 0,
/*4415*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BIT_CONVERT),
/*4418*/      OPC_MoveChild, 0,
/*4420*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*4423*/      OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*4425*/      OPC_CheckType, MVT::v16i8,
/*4427*/      OPC_MoveParent,
/*4428*/      OPC_MoveParent,
/*4429*/      OPC_RecordChild1, // #1 = $B
/*4430*/      OPC_MoveParent,
/*4431*/      OPC_CheckType, MVT::v4i32,
/*4433*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VANDC), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (and:v4i32 VRRC:v4i32:$A, (xor:v4i32 (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>), VRRC:v4i32:$B)) - Complexity = 13
              // Dst: (VANDC:v4i32 VRRC:v4i32:$A, VRRC:v4i32:$B)
/*4442*/    /*Scope*/ 69, /*->4512*/
/*4443*/      OPC_MoveChild, 0,
/*4445*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*4448*/      OPC_Scope, 30, /*->4480*/ // 2 children in Scope
/*4450*/        OPC_RecordChild0, // #0 = $B
/*4451*/        OPC_MoveChild, 1,
/*4453*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BIT_CONVERT),
/*4456*/        OPC_MoveChild, 0,
/*4458*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*4461*/        OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*4463*/        OPC_CheckType, MVT::v16i8,
/*4465*/        OPC_MoveParent,
/*4466*/        OPC_MoveParent,
/*4467*/        OPC_MoveParent,
/*4468*/        OPC_RecordChild1, // #1 = $A
/*4469*/        OPC_CheckType, MVT::v4i32,
/*4471*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VANDC), 0,
                    1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 1, 0, 
                // Src: (and:v4i32 (xor:v4i32 VRRC:v4i32:$B, (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>)), VRRC:v4i32:$A) - Complexity = 13
                // Dst: (VANDC:v4i32 VRRC:v4i32:$A, VRRC:v4i32:$B)
/*4480*/      /*Scope*/ 30, /*->4511*/
/*4481*/        OPC_MoveChild, 0,
/*4483*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BIT_CONVERT),
/*4486*/        OPC_MoveChild, 0,
/*4488*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*4491*/        OPC_CheckPredicate, 64, // Predicate_immAllOnesV
/*4493*/        OPC_CheckType, MVT::v16i8,
/*4495*/        OPC_MoveParent,
/*4496*/        OPC_MoveParent,
/*4497*/        OPC_RecordChild1, // #0 = $B
/*4498*/        OPC_MoveParent,
/*4499*/        OPC_RecordChild1, // #1 = $A
/*4500*/        OPC_CheckType, MVT::v4i32,
/*4502*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VANDC), 0,
                    1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 1, 0, 
                // Src: (and:v4i32 (xor:v4i32 (bitconvert:v4i32 (build_vector:v16i8)<<P:Predicate_immAllOnesV>>), VRRC:v4i32:$B), VRRC:v4i32:$A) - Complexity = 13
                // Dst: (VANDC:v4i32 VRRC:v4i32:$A, VRRC:v4i32:$B)
/*4511*/      0, /*End of Scope*/
/*4512*/    /*Scope*/ 13, /*->4526*/
/*4513*/      OPC_RecordChild0, // #0 = $vA
/*4514*/      OPC_RecordChild1, // #1 = $vB
/*4515*/      OPC_CheckType, MVT::v4i32,
/*4517*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VAND), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (and:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 3
              // Dst: (VAND:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*4526*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 2|128,2/*258*/,  TARGET_OPCODE(ISD::OR),// ->4789
/*4531*/    OPC_Scope, 46, /*->4579*/ // 3 children in Scope
/*4533*/      OPC_RecordChild0, // #0 = $rS
/*4534*/      OPC_MoveChild, 1,
/*4536*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*4539*/      OPC_RecordChild0, // #1 = $rB
/*4540*/      OPC_MoveChild, 1,
/*4542*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*4553*/      OPC_MoveParent,
/*4554*/      OPC_MoveParent,
/*4555*/      OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->4567
/*4558*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ORC), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (or:i32 GPRC:i32:$rS, (xor:i32 GPRC:i32:$rB, -1:i32)) - Complexity = 11
                // Dst: (ORC:i32 GPRC:i32:$rS, GPRC:i32:$rB)
              /*SwitchType*/ 9,  MVT::i64,// ->4578
/*4569*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ORC8), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (or:i64 G8RC:i64:$rS, (xor:i64 G8RC:i64:$rB, -1:i64)) - Complexity = 11
                // Dst: (ORC8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
              0, // EndSwitchType
/*4579*/    /*Scope*/ 46, /*->4626*/
/*4580*/      OPC_MoveChild, 0,
/*4582*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*4585*/      OPC_RecordChild0, // #0 = $rB
/*4586*/      OPC_MoveChild, 1,
/*4588*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*4599*/      OPC_MoveParent,
/*4600*/      OPC_MoveParent,
/*4601*/      OPC_RecordChild1, // #1 = $rS
/*4602*/      OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->4614
/*4605*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ORC), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (or:i32 (xor:i32 GPRC:i32:$rB, -1:i32), GPRC:i32:$rS) - Complexity = 11
                // Dst: (ORC:i32 GPRC:i32:$rS, GPRC:i32:$rB)
              /*SwitchType*/ 9,  MVT::i64,// ->4625
/*4616*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ORC8), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                // Src: (or:i64 (xor:i64 G8RC:i64:$rB, -1:i64), G8RC:i64:$rS) - Complexity = 11
                // Dst: (ORC8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
              0, // EndSwitchType
/*4626*/    /*Scope*/ 32|128,1/*160*/, /*->4788*/
/*4628*/      OPC_RecordChild0, // #0 = $src1
/*4629*/      OPC_RecordChild1, // #1 = $src2
/*4630*/      OPC_Scope, 119, /*->4751*/ // 4 children in Scope
/*4632*/        OPC_MoveChild, 1,
/*4634*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*4637*/        OPC_Scope, 19, /*->4658*/ // 5 children in Scope
/*4639*/          OPC_CheckPredicate, 62, // Predicate_immZExt16
/*4641*/          OPC_MoveParent,
/*4642*/          OPC_CheckType, MVT::i32,
/*4644*/          OPC_EmitConvertToTarget, 1,
/*4646*/          OPC_EmitNodeXForm, 1, 2, // LO16
/*4649*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ORI), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                  // Src: (or:i32 GPRC:i32:$src1, (imm:i32)<<P:Predicate_immZExt16>><<X:LO16>>:$src2) - Complexity = 7
                  // Dst: (ORI:i32 GPRC:i32:$src1, (LO16:i32 (imm:i32):$src2))
/*4658*/        /*Scope*/ 19, /*->4678*/
/*4659*/          OPC_CheckPredicate, 63, // Predicate_imm16ShiftedZExt
/*4661*/          OPC_MoveParent,
/*4662*/          OPC_CheckType, MVT::i32,
/*4664*/          OPC_EmitConvertToTarget, 1,
/*4666*/          OPC_EmitNodeXForm, 0, 2, // HI16
/*4669*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ORIS), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                  // Src: (or:i32 GPRC:i32:$src1, (imm:i32)<<P:Predicate_imm16ShiftedZExt>><<X:HI16>>:$src2) - Complexity = 7
                  // Dst: (ORIS:i32 GPRC:i32:$src1, (HI16:i32 (imm:i32):$src2))
/*4678*/        /*Scope*/ 19, /*->4698*/
/*4679*/          OPC_CheckPredicate, 62, // Predicate_immZExt16
/*4681*/          OPC_MoveParent,
/*4682*/          OPC_CheckType, MVT::i64,
/*4684*/          OPC_EmitConvertToTarget, 1,
/*4686*/          OPC_EmitNodeXForm, 1, 2, // LO16
/*4689*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ORI8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                  // Src: (or:i64 G8RC:i64:$src1, (imm:i64)<<P:Predicate_immZExt16>><<X:LO16>>:$src2) - Complexity = 7
                  // Dst: (ORI8:i64 G8RC:i64:$src1, (LO16:i32 (imm:i64):$src2))
/*4698*/        /*Scope*/ 19, /*->4718*/
/*4699*/          OPC_CheckPredicate, 63, // Predicate_imm16ShiftedZExt
/*4701*/          OPC_MoveParent,
/*4702*/          OPC_CheckType, MVT::i64,
/*4704*/          OPC_EmitConvertToTarget, 1,
/*4706*/          OPC_EmitNodeXForm, 0, 2, // HI16
/*4709*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ORIS8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                  // Src: (or:i64 G8RC:i64:$src1, (imm:i64)<<P:Predicate_imm16ShiftedZExt>><<X:HI16>>:$src2) - Complexity = 7
                  // Dst: (ORIS8:i64 G8RC:i64:$src1, (HI16:i32 (imm:i64):$src2))
/*4718*/        /*Scope*/ 31, /*->4750*/
/*4719*/          OPC_MoveParent,
/*4720*/          OPC_CheckType, MVT::i32,
/*4722*/          OPC_EmitConvertToTarget, 1,
/*4724*/          OPC_EmitNodeXForm, 1, 2, // LO16
/*4727*/          OPC_EmitNode, TARGET_OPCODE(PPC::ORI), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3,  // Results = #4 
/*4736*/          OPC_EmitConvertToTarget, 1,
/*4738*/          OPC_EmitNodeXForm, 0, 5, // HI16
/*4741*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ORIS), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 4, 6, 
                  // Src: (or:i32 GPRC:i32:$in, (imm:i32):$imm) - Complexity = 6
                  // Dst: (ORIS:i32 (ORI:i32 GPRC:i32:$in, (LO16:i32 (imm:i32):$imm)), (HI16:i32 (imm:i32):$imm))
/*4750*/        0, /*End of Scope*/
/*4751*/      /*Scope*/ 11, /*->4763*/
/*4752*/        OPC_CheckType, MVT::i32,
/*4754*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::OR), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (or:i32 GPRC:i32:$rS, GPRC:i32:$rB) - Complexity = 3
                // Dst: (OR:i32 GPRC:i32:$rS, GPRC:i32:$rB)
/*4763*/      /*Scope*/ 11, /*->4775*/
/*4764*/        OPC_CheckType, MVT::i64,
/*4766*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::OR8), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (or:i64 G8RC:i64:$rS, G8RC:i64:$rB) - Complexity = 3
                // Dst: (OR8:i64 G8RC:i64:$rS, G8RC:i64:$rB)
/*4775*/      /*Scope*/ 11, /*->4787*/
/*4776*/        OPC_CheckType, MVT::v4i32,
/*4778*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VOR), 0,
                    1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
                // Src: (or:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 3
                // Dst: (VOR:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*4787*/      0, /*End of Scope*/
/*4788*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 1|128,1/*129*/,  TARGET_OPCODE(PPCISD::Hi),// ->4922
/*4793*/    OPC_RecordChild0, // #0 = $in
/*4794*/    OPC_MoveChild, 0,
/*4796*/    OPC_SwitchOpcode /*4 cases */, 28,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->4828
/*4800*/      OPC_MoveParent,
/*4801*/      OPC_MoveChild, 1,
/*4803*/      OPC_CheckInteger, 0, 
/*4805*/      OPC_MoveParent,
/*4806*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->4817
/*4809*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LIS), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (PPChi:i32 (tglobaladdr:i32):$in, 0:i32) - Complexity = 11
                // Dst: (LIS:i32 (tglobaladdr:i32):$in)
              /*SwitchType*/ 8,  MVT::i64,// ->4827
/*4819*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LIS8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (PPChi:i64 (tglobaladdr:i64):$in, 0:i64) - Complexity = 11
                // Dst: (LIS8:i64 (tglobaladdr:i64):$in)
              0, // EndSwitchType
            /*SwitchOpcode*/ 28,  TARGET_OPCODE(ISD::TargetConstantPool),// ->4859
/*4831*/      OPC_MoveParent,
/*4832*/      OPC_MoveChild, 1,
/*4834*/      OPC_CheckInteger, 0, 
/*4836*/      OPC_MoveParent,
/*4837*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->4848
/*4840*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LIS), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (PPChi:i32 (tconstpool:i32):$in, 0:i32) - Complexity = 11
                // Dst: (LIS:i32 (tconstpool:i32):$in)
              /*SwitchType*/ 8,  MVT::i64,// ->4858
/*4850*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LIS8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (PPChi:i64 (tconstpool:i64):$in, 0:i64) - Complexity = 11
                // Dst: (LIS8:i64 (tconstpool:i64):$in)
              0, // EndSwitchType
            /*SwitchOpcode*/ 28,  TARGET_OPCODE(ISD::TargetJumpTable),// ->4890
/*4862*/      OPC_MoveParent,
/*4863*/      OPC_MoveChild, 1,
/*4865*/      OPC_CheckInteger, 0, 
/*4867*/      OPC_MoveParent,
/*4868*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->4879
/*4871*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LIS), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (PPChi:i32 (tjumptable:i32):$in, 0:i32) - Complexity = 11
                // Dst: (LIS:i32 (tjumptable:i32):$in)
              /*SwitchType*/ 8,  MVT::i64,// ->4889
/*4881*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LIS8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (PPChi:i64 (tjumptable:i64):$in, 0:i64) - Complexity = 11
                // Dst: (LIS8:i64 (tjumptable:i64):$in)
              0, // EndSwitchType
            /*SwitchOpcode*/ 28,  TARGET_OPCODE(ISD::TargetBlockAddress),// ->4921
/*4893*/      OPC_MoveParent,
/*4894*/      OPC_MoveChild, 1,
/*4896*/      OPC_CheckInteger, 0, 
/*4898*/      OPC_MoveParent,
/*4899*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->4910
/*4902*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LIS), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (PPChi:i32 (tblockaddress:i32):$in, 0:i32) - Complexity = 11
                // Dst: (LIS:i32 (tblockaddress:i32):$in)
              /*SwitchType*/ 8,  MVT::i64,// ->4920
/*4912*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LIS8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (PPChi:i64 (tblockaddress:i64):$in, 0:i64) - Complexity = 11
                // Dst: (LIS8:i64 (tblockaddress:i64):$in)
              0, // EndSwitchType
            0, // EndSwitchOpcode
          /*SwitchOpcode*/ 1|128,1/*129*/,  TARGET_OPCODE(PPCISD::Lo),// ->5055
/*4926*/    OPC_RecordChild0, // #0 = $in
/*4927*/    OPC_MoveChild, 0,
/*4929*/    OPC_SwitchOpcode /*4 cases */, 28,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->4961
/*4933*/      OPC_MoveParent,
/*4934*/      OPC_MoveChild, 1,
/*4936*/      OPC_CheckInteger, 0, 
/*4938*/      OPC_MoveParent,
/*4939*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->4950
/*4942*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LI), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (PPClo:i32 (tglobaladdr:i32):$in, 0:i32) - Complexity = 11
                // Dst: (LI:i32 (tglobaladdr:i32):$in)
              /*SwitchType*/ 8,  MVT::i64,// ->4960
/*4952*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LI8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (PPClo:i64 (tglobaladdr:i64):$in, 0:i64) - Complexity = 11
                // Dst: (LI8:i64 (tglobaladdr:i64):$in)
              0, // EndSwitchType
            /*SwitchOpcode*/ 28,  TARGET_OPCODE(ISD::TargetConstantPool),// ->4992
/*4964*/      OPC_MoveParent,
/*4965*/      OPC_MoveChild, 1,
/*4967*/      OPC_CheckInteger, 0, 
/*4969*/      OPC_MoveParent,
/*4970*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->4981
/*4973*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LI), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (PPClo:i32 (tconstpool:i32):$in, 0:i32) - Complexity = 11
                // Dst: (LI:i32 (tconstpool:i32):$in)
              /*SwitchType*/ 8,  MVT::i64,// ->4991
/*4983*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LI8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (PPClo:i64 (tconstpool:i64):$in, 0:i64) - Complexity = 11
                // Dst: (LI8:i64 (tconstpool:i64):$in)
              0, // EndSwitchType
            /*SwitchOpcode*/ 28,  TARGET_OPCODE(ISD::TargetJumpTable),// ->5023
/*4995*/      OPC_MoveParent,
/*4996*/      OPC_MoveChild, 1,
/*4998*/      OPC_CheckInteger, 0, 
/*5000*/      OPC_MoveParent,
/*5001*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->5012
/*5004*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LI), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (PPClo:i32 (tjumptable:i32):$in, 0:i32) - Complexity = 11
                // Dst: (LI:i32 (tjumptable:i32):$in)
              /*SwitchType*/ 8,  MVT::i64,// ->5022
/*5014*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LI8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (PPClo:i64 (tjumptable:i64):$in, 0:i64) - Complexity = 11
                // Dst: (LI8:i64 (tjumptable:i64):$in)
              0, // EndSwitchType
            /*SwitchOpcode*/ 28,  TARGET_OPCODE(ISD::TargetBlockAddress),// ->5054
/*5026*/      OPC_MoveParent,
/*5027*/      OPC_MoveChild, 1,
/*5029*/      OPC_CheckInteger, 0, 
/*5031*/      OPC_MoveParent,
/*5032*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->5043
/*5035*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LI), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (PPClo:i32 (tblockaddress:i32):$in, 0:i32) - Complexity = 11
                // Dst: (LI:i32 (tblockaddress:i32):$in)
              /*SwitchType*/ 8,  MVT::i64,// ->5053
/*5045*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LI8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (PPClo:i64 (tblockaddress:i64):$in, 0:i64) - Complexity = 11
                // Dst: (LI8:i64 (tblockaddress:i64):$in)
              0, // EndSwitchType
            0, // EndSwitchOpcode
          /*SwitchOpcode*/ 118,  TARGET_OPCODE(ISD::SUB),// ->5176
/*5058*/    OPC_Scope, 28, /*->5088*/ // 2 children in Scope
/*5060*/      OPC_MoveChild, 0,
/*5062*/      OPC_CheckInteger, 0, 
/*5064*/      OPC_MoveParent,
/*5065*/      OPC_RecordChild1, // #0 = $rA
/*5066*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->5077
/*5069*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::NEG), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (sub:i32 0:i32, GPRC:i32:$rA) - Complexity = 8
                // Dst: (NEG:i32 GPRC:i32:$rA)
              /*SwitchType*/ 8,  MVT::i64,// ->5087
/*5079*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::NEG8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (sub:i64 0:i64, G8RC:i64:$rA) - Complexity = 8
                // Dst: (NEG8:i64 G8RC:i64:$rA)
              0, // EndSwitchType
/*5088*/    /*Scope*/ 86, /*->5175*/
/*5089*/      OPC_RecordChild0, // #0 = $imm
/*5090*/      OPC_Scope, 23, /*->5115*/ // 2 children in Scope
/*5092*/        OPC_MoveChild, 0,
/*5094*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*5097*/        OPC_CheckPredicate, 0, // Predicate_immSExt16
/*5099*/        OPC_MoveParent,
/*5100*/        OPC_RecordChild1, // #1 = $in
/*5101*/        OPC_CheckType, MVT::i32,
/*5103*/        OPC_EmitConvertToTarget, 0,
/*5105*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFIC), 0,
                    2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 1, 2, 
                // Src: (sub:i32 (imm:i32)<<P:Predicate_immSExt16>>:$imm, GPRC:i32:$in) - Complexity = 7
                // Dst: (SUBFIC:i32:i32 GPRC:i32:$in, (imm:i32):$imm)
/*5115*/      /*Scope*/ 58, /*->5174*/
/*5116*/        OPC_RecordChild1, // #1 = $rA
/*5117*/        OPC_SwitchType /*5 cases */, 9,  MVT::i32,// ->5129
/*5120*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBF), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                  // Src: (sub:i32 GPRC:i32:$rB, GPRC:i32:$rA) - Complexity = 3
                  // Dst: (SUBF:i32 GPRC:i32:$rA, GPRC:i32:$rB)
                /*SwitchType*/ 9,  MVT::i64,// ->5140
/*5131*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBF8), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                  // Src: (sub:i64 G8RC:i64:$rB, G8RC:i64:$rA) - Complexity = 3
                  // Dst: (SUBF8:i64 G8RC:i64:$rA, G8RC:i64:$rB)
                /*SwitchType*/ 9,  MVT::v16i8,// ->5151
/*5142*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBUBM), 0,
                      1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
                  // Src: (sub:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 3
                  // Dst: (VSUBUBM:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
                /*SwitchType*/ 9,  MVT::v8i16,// ->5162
/*5153*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBUHM), 0,
                      1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
                  // Src: (sub:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 3
                  // Dst: (VSUBUHM:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
                /*SwitchType*/ 9,  MVT::v4i32,// ->5173
/*5164*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBUWM), 0,
                      1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
                  // Src: (sub:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 3
                  // Dst: (VSUBUWM:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
                0, // EndSwitchType
/*5174*/      0, /*End of Scope*/
/*5175*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 102,  TARGET_OPCODE(ISD::ADDE),// ->5281
/*5179*/    OPC_CaptureFlagInput,
/*5180*/    OPC_RecordChild0, // #0 = $rA
/*5181*/    OPC_Scope, 69, /*->5252*/ // 2 children in Scope
/*5183*/      OPC_MoveChild, 1,
/*5185*/      OPC_Scope, 36, /*->5223*/ // 2 children in Scope
/*5187*/        OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*5198*/        OPC_MoveParent,
/*5199*/        OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->5211
/*5202*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDME), 0|OPFL_FlagInput|OPFL_FlagOutput,
                      2/*#VTs*/, MVT::i32, MVT::i32, 1/*#Ops*/, 0, 
                  // Src: (adde:i32 GPRC:i32:$rA, -1:i32) - Complexity = 8
                  // Dst: (ADDME:i32:i32 GPRC:i32:$rA)
                /*SwitchType*/ 9,  MVT::i64,// ->5222
/*5213*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDME8), 0|OPFL_FlagInput|OPFL_FlagOutput,
                      2/*#VTs*/, MVT::i64, MVT::i32, 1/*#Ops*/, 0, 
                  // Src: (adde:i64 G8RC:i64:$rA, -1:i64) - Complexity = 8
                  // Dst: (ADDME8:i64:i32 G8RC:i64:$rA)
                0, // EndSwitchType
/*5223*/      /*Scope*/ 27, /*->5251*/
/*5224*/        OPC_CheckInteger, 0, 
/*5226*/        OPC_MoveParent,
/*5227*/        OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->5239
/*5230*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDZE), 0|OPFL_FlagInput|OPFL_FlagOutput,
                      2/*#VTs*/, MVT::i32, MVT::i32, 1/*#Ops*/, 0, 
                  // Src: (adde:i32 GPRC:i32:$rA, 0:i32) - Complexity = 8
                  // Dst: (ADDZE:i32:i32 GPRC:i32:$rA)
                /*SwitchType*/ 9,  MVT::i64,// ->5250
/*5241*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDZE8), 0|OPFL_FlagInput|OPFL_FlagOutput,
                      2/*#VTs*/, MVT::i64, MVT::i32, 1/*#Ops*/, 0, 
                  // Src: (adde:i64 G8RC:i64:$rA, 0:i64) - Complexity = 8
                  // Dst: (ADDZE8:i64:i32 G8RC:i64:$rA)
                0, // EndSwitchType
/*5251*/      0, /*End of Scope*/
/*5252*/    /*Scope*/ 27, /*->5280*/
/*5253*/      OPC_RecordChild1, // #1 = $rB
/*5254*/      OPC_SwitchType /*2 cases */, 10,  MVT::i32,// ->5267
/*5257*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDE), 0|OPFL_FlagInput|OPFL_FlagOutput,
                    2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (adde:i32 GPRC:i32:$rA, GPRC:i32:$rB) - Complexity = 3
                // Dst: (ADDE:i32:i32 GPRC:i32:$rA, GPRC:i32:$rB)
              /*SwitchType*/ 10,  MVT::i64,// ->5279
/*5269*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDE8), 0|OPFL_FlagInput|OPFL_FlagOutput,
                    2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (adde:i64 G8RC:i64:$rA, G8RC:i64:$rB) - Complexity = 3
                // Dst: (ADDE8:i64:i32 G8RC:i64:$rA, G8RC:i64:$rB)
              0, // EndSwitchType
/*5280*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 104,  TARGET_OPCODE(ISD::SUBE),// ->5388
/*5284*/    OPC_CaptureFlagInput,
/*5285*/    OPC_Scope, 71, /*->5358*/ // 2 children in Scope
/*5287*/      OPC_MoveChild, 0,
/*5289*/      OPC_Scope, 37, /*->5328*/ // 2 children in Scope
/*5291*/        OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*5302*/        OPC_MoveParent,
/*5303*/        OPC_RecordChild1, // #0 = $rA
/*5304*/        OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->5316
/*5307*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFME), 0|OPFL_FlagInput|OPFL_FlagOutput,
                      2/*#VTs*/, MVT::i32, MVT::i32, 1/*#Ops*/, 0, 
                  // Src: (sube:i32 -1:i32, GPRC:i32:$rA) - Complexity = 8
                  // Dst: (SUBFME:i32:i32 GPRC:i32:$rA)
                /*SwitchType*/ 9,  MVT::i64,// ->5327
/*5318*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFME8), 0|OPFL_FlagInput|OPFL_FlagOutput,
                      2/*#VTs*/, MVT::i64, MVT::i32, 1/*#Ops*/, 0, 
                  // Src: (sube:i64 -1:i64, G8RC:i64:$rA) - Complexity = 8
                  // Dst: (SUBFME8:i64:i32 G8RC:i64:$rA)
                0, // EndSwitchType
/*5328*/      /*Scope*/ 28, /*->5357*/
/*5329*/        OPC_CheckInteger, 0, 
/*5331*/        OPC_MoveParent,
/*5332*/        OPC_RecordChild1, // #0 = $rA
/*5333*/        OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->5345
/*5336*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFZE), 0|OPFL_FlagInput|OPFL_FlagOutput,
                      2/*#VTs*/, MVT::i32, MVT::i32, 1/*#Ops*/, 0, 
                  // Src: (sube:i32 0:i32, GPRC:i32:$rA) - Complexity = 8
                  // Dst: (SUBFZE:i32:i32 GPRC:i32:$rA)
                /*SwitchType*/ 9,  MVT::i64,// ->5356
/*5347*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFZE8), 0|OPFL_FlagInput|OPFL_FlagOutput,
                      2/*#VTs*/, MVT::i64, MVT::i32, 1/*#Ops*/, 0, 
                  // Src: (sube:i64 0:i64, G8RC:i64:$rA) - Complexity = 8
                  // Dst: (SUBFZE8:i64:i32 G8RC:i64:$rA)
                0, // EndSwitchType
/*5357*/      0, /*End of Scope*/
/*5358*/    /*Scope*/ 28, /*->5387*/
/*5359*/      OPC_RecordChild0, // #0 = $rB
/*5360*/      OPC_RecordChild1, // #1 = $rA
/*5361*/      OPC_SwitchType /*2 cases */, 10,  MVT::i32,// ->5374
/*5364*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFE), 0|OPFL_FlagInput|OPFL_FlagOutput,
                    2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (sube:i32 GPRC:i32:$rB, GPRC:i32:$rA) - Complexity = 3
                // Dst: (SUBFE:i32:i32 GPRC:i32:$rA, GPRC:i32:$rB)
              /*SwitchType*/ 10,  MVT::i64,// ->5386
/*5376*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFE8), 0|OPFL_FlagInput|OPFL_FlagOutput,
                    2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (sube:i64 G8RC:i64:$rB, G8RC:i64:$rA) - Complexity = 3
                // Dst: (SUBFE8:i64:i32 G8RC:i64:$rA, G8RC:i64:$rB)
              0, // EndSwitchType
/*5387*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 69,  TARGET_OPCODE(ISD::ADDC),// ->5460
/*5391*/    OPC_RecordChild0, // #0 = $rA
/*5392*/    OPC_RecordChild1, // #1 = $imm
/*5393*/    OPC_Scope, 38, /*->5433*/ // 3 children in Scope
/*5395*/      OPC_MoveChild, 1,
/*5397*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*5400*/      OPC_CheckPredicate, 0, // Predicate_immSExt16
/*5402*/      OPC_MoveParent,
/*5403*/      OPC_SwitchType /*2 cases */, 12,  MVT::i32,// ->5418
/*5406*/        OPC_EmitConvertToTarget, 1,
/*5408*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIC), 0|OPFL_FlagOutput,
                    2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 0, 2, 
                // Src: (addc:i32 GPRC:i32:$rA, (imm:i32)<<P:Predicate_immSExt16>>:$imm) - Complexity = 7
                // Dst: (ADDIC:i32:i32 GPRC:i32:$rA, (imm:i32):$imm)
              /*SwitchType*/ 12,  MVT::i64,// ->5432
/*5420*/        OPC_EmitConvertToTarget, 1,
/*5422*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDIC8), 0|OPFL_FlagOutput,
                    2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 0, 2, 
                // Src: (addc:i64 G8RC:i64:$rA, (imm:i64)<<P:Predicate_immSExt16>>:$imm) - Complexity = 7
                // Dst: (ADDIC8:i64:i32 G8RC:i64:$rA, (imm:i64):$imm)
              0, // EndSwitchType
/*5433*/    /*Scope*/ 12, /*->5446*/
/*5434*/      OPC_CheckType, MVT::i32,
/*5436*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDC), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (addc:i32 GPRC:i32:$rA, GPRC:i32:$rB) - Complexity = 3
              // Dst: (ADDC:i32:i32 GPRC:i32:$rA, GPRC:i32:$rB)
/*5446*/    /*Scope*/ 12, /*->5459*/
/*5447*/      OPC_CheckType, MVT::i64,
/*5449*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADDC8), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (addc:i64 G8RC:i64:$rA, G8RC:i64:$rB) - Complexity = 3
              // Dst: (ADDC8:i64:i32 G8RC:i64:$rA, G8RC:i64:$rB)
/*5459*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 50,  TARGET_OPCODE(ISD::MUL),// ->5513
/*5463*/    OPC_RecordChild0, // #0 = $rA
/*5464*/    OPC_RecordChild1, // #1 = $imm
/*5465*/    OPC_Scope, 21, /*->5488*/ // 3 children in Scope
/*5467*/      OPC_MoveChild, 1,
/*5469*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*5472*/      OPC_CheckPredicate, 0, // Predicate_immSExt16
/*5474*/      OPC_MoveParent,
/*5475*/      OPC_CheckType, MVT::i32,
/*5477*/      OPC_EmitConvertToTarget, 1,
/*5479*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::MULLI), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
              // Src: (mul:i32 GPRC:i32:$rA, (imm:i32)<<P:Predicate_immSExt16>>:$imm) - Complexity = 7
              // Dst: (MULLI:i32 GPRC:i32:$rA, (imm:i32):$imm)
/*5488*/    /*Scope*/ 11, /*->5500*/
/*5489*/      OPC_CheckType, MVT::i32,
/*5491*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::MULLW), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (mul:i32 GPRC:i32:$rA, GPRC:i32:$rB) - Complexity = 3
              // Dst: (MULLW:i32 GPRC:i32:$rA, GPRC:i32:$rB)
/*5500*/    /*Scope*/ 11, /*->5512*/
/*5501*/      OPC_CheckType, MVT::i64,
/*5503*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::MULLD), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (mul:i64 G8RC:i64:$rA, G8RC:i64:$rB) - Complexity = 3
              // Dst: (MULLD:i64 G8RC:i64:$rA, G8RC:i64:$rB)
/*5512*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 71,  TARGET_OPCODE(ISD::SUBC),// ->5587
/*5516*/    OPC_RecordChild0, // #0 = $imm
/*5517*/    OPC_Scope, 39, /*->5558*/ // 2 children in Scope
/*5519*/      OPC_MoveChild, 0,
/*5521*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*5524*/      OPC_CheckPredicate, 0, // Predicate_immSExt16
/*5526*/      OPC_MoveParent,
/*5527*/      OPC_RecordChild1, // #1 = $rA
/*5528*/      OPC_SwitchType /*2 cases */, 12,  MVT::i32,// ->5543
/*5531*/        OPC_EmitConvertToTarget, 0,
/*5533*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFIC), 0|OPFL_FlagOutput,
                    2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 1, 2, 
                // Src: (subc:i32 (imm:i32)<<P:Predicate_immSExt16>>:$imm, GPRC:i32:$rA) - Complexity = 7
                // Dst: (SUBFIC:i32:i32 GPRC:i32:$rA, (imm:i32):$imm)
              /*SwitchType*/ 12,  MVT::i64,// ->5557
/*5545*/        OPC_EmitConvertToTarget, 0,
/*5547*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFIC8), 0|OPFL_FlagOutput,
                    2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 1, 2, 
                // Src: (subc:i64 (imm:i64)<<P:Predicate_immSExt16>>:$imm, G8RC:i64:$rA) - Complexity = 7
                // Dst: (SUBFIC8:i64:i32 G8RC:i64:$rA, (imm:i64):$imm)
              0, // EndSwitchType
/*5558*/    /*Scope*/ 27, /*->5586*/
/*5559*/      OPC_RecordChild1, // #1 = $rA
/*5560*/      OPC_SwitchType /*2 cases */, 10,  MVT::i32,// ->5573
/*5563*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFC), 0|OPFL_FlagOutput,
                    2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (subc:i32 GPRC:i32:$rB, GPRC:i32:$rA) - Complexity = 3
                // Dst: (SUBFC:i32:i32 GPRC:i32:$rA, GPRC:i32:$rB)
              /*SwitchType*/ 10,  MVT::i64,// ->5585
/*5575*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SUBFC8), 0|OPFL_FlagOutput,
                    2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (subc:i64 G8RC:i64:$rB, G8RC:i64:$rA) - Complexity = 3
                // Dst: (SUBFC8:i64:i32 G8RC:i64:$rA, G8RC:i64:$rB)
              0, // EndSwitchType
/*5586*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 114,  TARGET_OPCODE(ISD::SRA),// ->5704
/*5590*/    OPC_RecordChild0, // #0 = $rS
/*5591*/    OPC_RecordChild1, // #1 = $SH
/*5592*/    OPC_Scope, 38, /*->5632*/ // 5 children in Scope
/*5594*/      OPC_MoveChild, 1,
/*5596*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*5599*/      OPC_CheckType, MVT::i32,
/*5601*/      OPC_MoveParent,
/*5602*/      OPC_SwitchType /*2 cases */, 12,  MVT::i32,// ->5617
/*5605*/        OPC_EmitConvertToTarget, 1,
/*5607*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SRAWI), 0,
                    2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 0, 2, 
                // Src: (sra:i32 GPRC:i32:$rS, (imm:i32):$SH) - Complexity = 6
                // Dst: (SRAWI:i32:i32 GPRC:i32:$rS, (imm:i32):$SH)
              /*SwitchType*/ 12,  MVT::i64,// ->5631
/*5619*/        OPC_EmitConvertToTarget, 1,
/*5621*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SRADI), 0,
                    2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 0, 2, 
                // Src: (sra:i64 G8RC:i64:$rS, (imm:i32):$SH) - Complexity = 6
                // Dst: (SRADI:i64:i32 G8RC:i64:$rS, (imm:i32):$SH)
              0, // EndSwitchType
/*5632*/    /*Scope*/ 28, /*->5661*/
/*5633*/      OPC_CheckChild1Type, MVT::i32,
/*5635*/      OPC_SwitchType /*2 cases */, 10,  MVT::i32,// ->5648
/*5638*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SRAW), 0,
                    2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (sra:i32 GPRC:i32:$rS, GPRC:i32:$rB) - Complexity = 3
                // Dst: (SRAW:i32:i32 GPRC:i32:$rS, GPRC:i32:$rB)
              /*SwitchType*/ 10,  MVT::i64,// ->5660
/*5650*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SRAD), 0,
                    2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (sra:i64 G8RC:i64:$rS, GPRC:i32:$rB) - Complexity = 3
                // Dst: (SRAD:i64:i32 G8RC:i64:$rS, GPRC:i32:$rB)
              0, // EndSwitchType
/*5661*/    /*Scope*/ 13, /*->5675*/
/*5662*/      OPC_CheckChild1Type, MVT::v16i8,
/*5664*/      OPC_CheckType, MVT::v16i8,
/*5666*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRAB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (sra:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 3
              // Dst: (VSRAB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*5675*/    /*Scope*/ 13, /*->5689*/
/*5676*/      OPC_CheckChild1Type, MVT::v8i16,
/*5678*/      OPC_CheckType, MVT::v8i16,
/*5680*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRAH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (sra:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 3
              // Dst: (VSRAH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*5689*/    /*Scope*/ 13, /*->5703*/
/*5690*/      OPC_CheckChild1Type, MVT::v4i32,
/*5692*/      OPC_CheckType, MVT::v4i32,
/*5694*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRAW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (sra:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 3
              // Dst: (VSRAW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*5703*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 126,  TARGET_OPCODE(ISD::SHL),// ->5833
/*5707*/    OPC_RecordChild0, // #0 = $in
/*5708*/    OPC_RecordChild1, // #1 = $imm
/*5709*/    OPC_Scope, 52, /*->5763*/ // 5 children in Scope
/*5711*/      OPC_MoveChild, 1,
/*5713*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*5716*/      OPC_CheckType, MVT::i32,
/*5718*/      OPC_MoveParent,
/*5719*/      OPC_SwitchType /*2 cases */, 21,  MVT::i32,// ->5743
/*5722*/        OPC_EmitConvertToTarget, 1,
/*5724*/        OPC_EmitInteger, MVT::i32, 0, 
/*5727*/        OPC_EmitConvertToTarget, 1,
/*5729*/        OPC_EmitNodeXForm, 5, 4, // SHL32
/*5732*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::RLWINM), 0,
                    1/*#VTs*/, MVT::i32, 4/*#Ops*/, 0, 2, 3, 5, 
                // Src: (shl:i32 GPRC:i32:$in, (imm:i32):$imm) - Complexity = 6
                // Dst: (RLWINM:i32 GPRC:i32:$in, (imm:i32):$imm, 0:i32, (SHL32:i32 (imm:i32):$imm))
              /*SwitchType*/ 17,  MVT::i64,// ->5762
/*5745*/        OPC_EmitConvertToTarget, 1,
/*5747*/        OPC_EmitConvertToTarget, 1,
/*5749*/        OPC_EmitNodeXForm, 6, 3, // SHL64
/*5752*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::RLDICR), 0,
                    1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 2, 4, 
                // Src: (shl:i64 G8RC:i64:$in, (imm:i32):$imm) - Complexity = 6
                // Dst: (RLDICR:i64 G8RC:i64:$in, (imm:i32):$imm, (SHL64:i32 (imm:i32):$imm))
              0, // EndSwitchType
/*5763*/    /*Scope*/ 26, /*->5790*/
/*5764*/      OPC_CheckChild1Type, MVT::i32,
/*5766*/      OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->5778
/*5769*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SLW), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (shl:i32 GPRC:i32:$rS, GPRC:i32:$rB) - Complexity = 3
                // Dst: (SLW:i32 GPRC:i32:$rS, GPRC:i32:$rB)
              /*SwitchType*/ 9,  MVT::i64,// ->5789
/*5780*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SLD), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (shl:i64 G8RC:i64:$rS, GPRC:i32:$rB) - Complexity = 3
                // Dst: (SLD:i64 G8RC:i64:$rS, GPRC:i32:$rB)
              0, // EndSwitchType
/*5790*/    /*Scope*/ 13, /*->5804*/
/*5791*/      OPC_CheckChild1Type, MVT::v16i8,
/*5793*/      OPC_CheckType, MVT::v16i8,
/*5795*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSLB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (shl:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 3
              // Dst: (VSLB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*5804*/    /*Scope*/ 13, /*->5818*/
/*5805*/      OPC_CheckChild1Type, MVT::v8i16,
/*5807*/      OPC_CheckType, MVT::v8i16,
/*5809*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSLH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (shl:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 3
              // Dst: (VSLH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*5818*/    /*Scope*/ 13, /*->5832*/
/*5819*/      OPC_CheckChild1Type, MVT::v4i32,
/*5821*/      OPC_CheckType, MVT::v4i32,
/*5823*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSLW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (shl:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 3
              // Dst: (VSLW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*5832*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 126,  TARGET_OPCODE(ISD::SRL),// ->5962
/*5836*/    OPC_RecordChild0, // #0 = $in
/*5837*/    OPC_RecordChild1, // #1 = $imm
/*5838*/    OPC_Scope, 52, /*->5892*/ // 5 children in Scope
/*5840*/      OPC_MoveChild, 1,
/*5842*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*5845*/      OPC_CheckType, MVT::i32,
/*5847*/      OPC_MoveParent,
/*5848*/      OPC_SwitchType /*2 cases */, 21,  MVT::i32,// ->5872
/*5851*/        OPC_EmitConvertToTarget, 1,
/*5853*/        OPC_EmitNodeXForm, 7, 2, // SRL32
/*5856*/        OPC_EmitConvertToTarget, 1,
/*5858*/        OPC_EmitInteger, MVT::i32, 31, 
/*5861*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::RLWINM), 0,
                    1/*#VTs*/, MVT::i32, 4/*#Ops*/, 0, 3, 4, 5, 
                // Src: (srl:i32 GPRC:i32:$in, (imm:i32):$imm) - Complexity = 6
                // Dst: (RLWINM:i32 GPRC:i32:$in, (SRL32:i32 (imm:i32):$imm), (imm:i32):$imm, 31:i32)
              /*SwitchType*/ 17,  MVT::i64,// ->5891
/*5874*/        OPC_EmitConvertToTarget, 1,
/*5876*/        OPC_EmitNodeXForm, 8, 2, // SRL64
/*5879*/        OPC_EmitConvertToTarget, 1,
/*5881*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::RLDICL), 0,
                    1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 4, 
                // Src: (srl:i64 G8RC:i64:$in, (imm:i32):$imm) - Complexity = 6
                // Dst: (RLDICL:i64 G8RC:i64:$in, (SRL64:i32 (imm:i32):$imm), (imm:i32):$imm)
              0, // EndSwitchType
/*5892*/    /*Scope*/ 26, /*->5919*/
/*5893*/      OPC_CheckChild1Type, MVT::i32,
/*5895*/      OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->5907
/*5898*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SRW), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (srl:i32 GPRC:i32:$rS, GPRC:i32:$rB) - Complexity = 3
                // Dst: (SRW:i32 GPRC:i32:$rS, GPRC:i32:$rB)
              /*SwitchType*/ 9,  MVT::i64,// ->5918
/*5909*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::SRD), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (srl:i64 G8RC:i64:$rS, GPRC:i32:$rB) - Complexity = 3
                // Dst: (SRD:i64 G8RC:i64:$rS, GPRC:i32:$rB)
              0, // EndSwitchType
/*5919*/    /*Scope*/ 13, /*->5933*/
/*5920*/      OPC_CheckChild1Type, MVT::v16i8,
/*5922*/      OPC_CheckType, MVT::v16i8,
/*5924*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (srl:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 3
              // Dst: (VSRB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*5933*/    /*Scope*/ 13, /*->5947*/
/*5934*/      OPC_CheckChild1Type, MVT::v8i16,
/*5936*/      OPC_CheckType, MVT::v8i16,
/*5938*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (srl:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 3
              // Dst: (VSRH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*5947*/    /*Scope*/ 13, /*->5961*/
/*5948*/      OPC_CheckChild1Type, MVT::v4i32,
/*5950*/      OPC_CheckType, MVT::v4i32,
/*5952*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (srl:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 3
              // Dst: (VSRW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*5961*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 92,  TARGET_OPCODE(ISD::ROTL),// ->6057
/*5965*/    OPC_RecordChild0, // #0 = $in
/*5966*/    OPC_RecordChild1, // #1 = $imm
/*5967*/    OPC_Scope, 48, /*->6017*/ // 2 children in Scope
/*5969*/      OPC_MoveChild, 1,
/*5971*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*5974*/      OPC_CheckType, MVT::i32,
/*5976*/      OPC_MoveParent,
/*5977*/      OPC_SwitchType /*2 cases */, 19,  MVT::i32,// ->5999
/*5980*/        OPC_EmitConvertToTarget, 1,
/*5982*/        OPC_EmitInteger, MVT::i32, 0, 
/*5985*/        OPC_EmitInteger, MVT::i32, 31, 
/*5988*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::RLWINM), 0,
                    1/*#VTs*/, MVT::i32, 4/*#Ops*/, 0, 2, 3, 4, 
                // Src: (rotl:i32 GPRC:i32:$in, (imm:i32):$imm) - Complexity = 6
                // Dst: (RLWINM:i32 GPRC:i32:$in, (imm:i32):$imm, 0:i32, 31:i32)
              /*SwitchType*/ 15,  MVT::i64,// ->6016
/*6001*/        OPC_EmitConvertToTarget, 1,
/*6003*/        OPC_EmitInteger, MVT::i32, 0, 
/*6006*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::RLDICL), 0,
                    1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 2, 3, 
                // Src: (rotl:i64 G8RC:i64:$in, (imm:i32):$imm) - Complexity = 6
                // Dst: (RLDICL:i64 G8RC:i64:$in, (imm:i32):$imm, 0:i32)
              0, // EndSwitchType
/*6017*/    /*Scope*/ 38, /*->6056*/
/*6018*/      OPC_CheckChild1Type, MVT::i32,
/*6020*/      OPC_SwitchType /*2 cases */, 17,  MVT::i32,// ->6040
/*6023*/        OPC_EmitInteger, MVT::i32, 0, 
/*6026*/        OPC_EmitInteger, MVT::i32, 31, 
/*6029*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::RLWNM), 0,
                    1/*#VTs*/, MVT::i32, 4/*#Ops*/, 0, 1, 2, 3, 
                // Src: (rotl:i32 GPRC:i32:$in, GPRC:i32:$sh) - Complexity = 3
                // Dst: (RLWNM:i32 GPRC:i32:$in, GPRC:i32:$sh, 0:i32, 31:i32)
              /*SwitchType*/ 13,  MVT::i64,// ->6055
/*6042*/        OPC_EmitInteger, MVT::i32, 0, 
/*6045*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::RLDCL), 0,
                    1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 1, 2, 
                // Src: (rotl:i64 G8RC:i64:$in, GPRC:i32:$sh) - Complexity = 3
                // Dst: (RLDCL:i64 G8RC:i64:$in, GPRC:i32:$sh, 0:i32)
              0, // EndSwitchType
/*6056*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 19,  TARGET_OPCODE(PPCISD::TOC_ENTRY),// ->6079
/*6060*/    OPC_RecordChild0, // #0 = $disp
/*6061*/    OPC_MoveChild, 0,
/*6063*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetGlobalAddress),
/*6066*/    OPC_MoveParent,
/*6067*/    OPC_RecordChild1, // #1 = $reg
/*6068*/    OPC_CheckType, MVT::i64,
/*6070*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::LDtoc), 0,
                1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
            // Src: (PPCtoc_entry:i64 (tglobaladdr:i64):$disp, G8RC:i64:$reg) - Complexity = 6
            // Dst: (LDtoc:i64 (tglobaladdr:i64):$disp, G8RC:i64:$reg)
          /*SwitchOpcode*/ 97,  TARGET_OPCODE(ISD::Constant),// ->6179
/*6082*/    OPC_RecordNode,   // #0 = $imm
/*6083*/    OPC_SwitchType /*2 cases */, 59,  MVT::i32,// ->6145
/*6086*/      OPC_Scope, 12, /*->6100*/ // 3 children in Scope
/*6088*/        OPC_CheckPredicate, 0, // Predicate_immSExt16
/*6090*/        OPC_EmitConvertToTarget, 0,
/*6092*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LI), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
                // Src: (imm:i32)<<P:Predicate_immSExt16>>:$imm - Complexity = 4
                // Dst: (LI:i32 (imm:i32):$imm)
/*6100*/      /*Scope*/ 15, /*->6116*/
/*6101*/        OPC_CheckPredicate, 1, // Predicate_imm16ShiftedSExt
/*6103*/        OPC_EmitConvertToTarget, 0,
/*6105*/        OPC_EmitNodeXForm, 0, 1, // HI16
/*6108*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LIS), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2, 
                // Src: (imm:i32)<<P:Predicate_imm16ShiftedSExt>><<X:HI16>>:$imm - Complexity = 4
                // Dst: (LIS:i32 (HI16:i32 (imm:i32):$imm))
/*6116*/      /*Scope*/ 27, /*->6144*/
/*6117*/        OPC_EmitConvertToTarget, 0,
/*6119*/        OPC_EmitNodeXForm, 0, 1, // HI16
/*6122*/        OPC_EmitNode, TARGET_OPCODE(PPC::LIS), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2,  // Results = #3 
/*6130*/        OPC_EmitConvertToTarget, 0,
/*6132*/        OPC_EmitNodeXForm, 1, 4, // LO16
/*6135*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::ORI), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 3, 5, 
                // Src: (imm:i32):$imm - Complexity = 3
                // Dst: (ORI:i32 (LIS:i32 (HI16:i32 (imm:i32):$imm)), (LO16:i32 (imm:i32):$imm))
/*6144*/      0, /*End of Scope*/
            /*SwitchType*/ 31,  MVT::i64,// ->6178
/*6147*/      OPC_Scope, 12, /*->6161*/ // 2 children in Scope
/*6149*/        OPC_CheckPredicate, 0, // Predicate_immSExt16
/*6151*/        OPC_EmitConvertToTarget, 0,
/*6153*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LI8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 1, 
                // Src: (imm:i64)<<P:Predicate_immSExt16>>:$imm - Complexity = 4
                // Dst: (LI8:i64 (imm:i64):$imm)
/*6161*/      /*Scope*/ 15, /*->6177*/
/*6162*/        OPC_CheckPredicate, 1, // Predicate_imm16ShiftedSExt
/*6164*/        OPC_EmitConvertToTarget, 0,
/*6166*/        OPC_EmitNodeXForm, 0, 1, // HI16
/*6169*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::LIS8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 2, 
                // Src: (imm:i64)<<P:Predicate_imm16ShiftedSExt>><<X:HI16>>:$imm - Complexity = 4
                // Dst: (LIS8:i64 (HI16:i64 (imm:i64):$imm))
/*6177*/      0, /*End of Scope*/
            0, // EndSwitchType
          /*SwitchOpcode*/ 28,  TARGET_OPCODE(PPCISD::SHL),// ->6210
/*6182*/    OPC_RecordChild0, // #0 = $rS
/*6183*/    OPC_RecordChild1, // #1 = $rB
/*6184*/    OPC_CheckChild1Type, MVT::i32,
/*6186*/    OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->6198
/*6189*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::SLW), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCshl:i32 GPRC:i32:$rS, GPRC:i32:$rB) - Complexity = 3
              // Dst: (SLW:i32 GPRC:i32:$rS, GPRC:i32:$rB)
            /*SwitchType*/ 9,  MVT::i64,// ->6209
/*6200*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::SLD), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (PPCshl:i64 G8RC:i64:$rS, GPRC:i32:$rB) - Complexity = 3
              // Dst: (SLD:i64 G8RC:i64:$rS, GPRC:i32:$rB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 28,  TARGET_OPCODE(PPCISD::SRL),// ->6241
/*6213*/    OPC_RecordChild0, // #0 = $rS
/*6214*/    OPC_RecordChild1, // #1 = $rB
/*6215*/    OPC_CheckChild1Type, MVT::i32,
/*6217*/    OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->6229
/*6220*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::SRW), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCsrl:i32 GPRC:i32:$rS, GPRC:i32:$rB) - Complexity = 3
              // Dst: (SRW:i32 GPRC:i32:$rS, GPRC:i32:$rB)
            /*SwitchType*/ 9,  MVT::i64,// ->6240
/*6231*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::SRD), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (PPCsrl:i64 G8RC:i64:$rS, GPRC:i32:$rB) - Complexity = 3
              // Dst: (SRD:i64 G8RC:i64:$rS, GPRC:i32:$rB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 30,  TARGET_OPCODE(PPCISD::SRA),// ->6274
/*6244*/    OPC_RecordChild0, // #0 = $rS
/*6245*/    OPC_RecordChild1, // #1 = $rB
/*6246*/    OPC_CheckChild1Type, MVT::i32,
/*6248*/    OPC_SwitchType /*2 cases */, 10,  MVT::i32,// ->6261
/*6251*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::SRAW), 0,
                  2/*#VTs*/, MVT::i32, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCsra:i32 GPRC:i32:$rS, GPRC:i32:$rB) - Complexity = 3
              // Dst: (SRAW:i32:i32 GPRC:i32:$rS, GPRC:i32:$rB)
            /*SwitchType*/ 10,  MVT::i64,// ->6273
/*6263*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::SRAD), 0,
                  2/*#VTs*/, MVT::i64, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCsra:i64 G8RC:i64:$rS, GPRC:i32:$rB) - Complexity = 3
              // Dst: (SRAD:i64:i32 G8RC:i64:$rS, GPRC:i32:$rB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 23,  TARGET_OPCODE(ISD::CTLZ),// ->6300
/*6277*/    OPC_RecordChild0, // #0 = $rS
/*6278*/    OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->6289
/*6281*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::CNTLZW), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (ctlz:i32 GPRC:i32:$rS) - Complexity = 3
              // Dst: (CNTLZW:i32 GPRC:i32:$rS)
            /*SwitchType*/ 8,  MVT::i64,// ->6299
/*6291*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::CNTLZD), 0,
                  1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
              // Src: (ctlz:i64 G8RC:i64:$rS) - Complexity = 3
              // Dst: (CNTLZD:i64 G8RC:i64:$rS)
            0, // EndSwitchType
          /*SwitchOpcode*/ 69,  TARGET_OPCODE(ISD::SIGN_EXTEND_INREG),// ->6372
/*6303*/    OPC_RecordChild0, // #0 = $rS
/*6304*/    OPC_MoveChild, 1,
/*6306*/    OPC_Scope, 25, /*->6333*/ // 3 children in Scope
/*6308*/      OPC_CheckValueType, MVT::i8,
/*6310*/      OPC_MoveParent,
/*6311*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->6322
/*6314*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::EXTSB), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (sext_inreg:i32 GPRC:i32:$rS, i8:Other) - Complexity = 3
                // Dst: (EXTSB:i32 GPRC:i32:$rS)
              /*SwitchType*/ 8,  MVT::i64,// ->6332
/*6324*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::EXTSB8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (sext_inreg:i64 G8RC:i64:$rS, i8:Other) - Complexity = 3
                // Dst: (EXTSB8:i64 G8RC:i64:$rS)
              0, // EndSwitchType
/*6333*/    /*Scope*/ 25, /*->6359*/
/*6334*/      OPC_CheckValueType, MVT::i16,
/*6336*/      OPC_MoveParent,
/*6337*/      OPC_SwitchType /*2 cases */, 8,  MVT::i32,// ->6348
/*6340*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::EXTSH), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
                // Src: (sext_inreg:i32 GPRC:i32:$rS, i16:Other) - Complexity = 3
                // Dst: (EXTSH:i32 GPRC:i32:$rS)
              /*SwitchType*/ 8,  MVT::i64,// ->6358
/*6350*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::EXTSH8), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (sext_inreg:i64 G8RC:i64:$rS, i16:Other) - Complexity = 3
                // Dst: (EXTSH8:i64 G8RC:i64:$rS)
              0, // EndSwitchType
/*6359*/    /*Scope*/ 11, /*->6371*/
/*6360*/      OPC_CheckValueType, MVT::i32,
/*6362*/      OPC_MoveParent,
/*6363*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::EXTSW), 0,
                  1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
              // Src: (sext_inreg:i64 G8RC:i64:$rS, i32:Other) - Complexity = 3
              // Dst: (EXTSW:i64 G8RC:i64:$rS)
/*6371*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::SDIV),// ->6401
/*6375*/    OPC_RecordChild0, // #0 = $rA
/*6376*/    OPC_RecordChild1, // #1 = $rB
/*6377*/    OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->6389
/*6380*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::DIVW), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (sdiv:i32 GPRC:i32:$rA, GPRC:i32:$rB) - Complexity = 3
              // Dst: (DIVW:i32 GPRC:i32:$rA, GPRC:i32:$rB)
            /*SwitchType*/ 9,  MVT::i64,// ->6400
/*6391*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::DIVD), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (sdiv:i64 G8RC:i64:$rA, G8RC:i64:$rB) - Complexity = 3
              // Dst: (DIVD:i64 G8RC:i64:$rA, G8RC:i64:$rB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::UDIV),// ->6430
/*6404*/    OPC_RecordChild0, // #0 = $rA
/*6405*/    OPC_RecordChild1, // #1 = $rB
/*6406*/    OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->6418
/*6409*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::DIVWU), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (udiv:i32 GPRC:i32:$rA, GPRC:i32:$rB) - Complexity = 3
              // Dst: (DIVWU:i32 GPRC:i32:$rA, GPRC:i32:$rB)
            /*SwitchType*/ 9,  MVT::i64,// ->6429
/*6420*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::DIVDU), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (udiv:i64 G8RC:i64:$rA, G8RC:i64:$rB) - Complexity = 3
              // Dst: (DIVDU:i64 G8RC:i64:$rA, G8RC:i64:$rB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::MULHS),// ->6459
/*6433*/    OPC_RecordChild0, // #0 = $rA
/*6434*/    OPC_RecordChild1, // #1 = $rB
/*6435*/    OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->6447
/*6438*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::MULHW), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (mulhs:i32 GPRC:i32:$rA, GPRC:i32:$rB) - Complexity = 3
              // Dst: (MULHW:i32 GPRC:i32:$rA, GPRC:i32:$rB)
            /*SwitchType*/ 9,  MVT::i64,// ->6458
/*6449*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::MULHD), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (mulhs:i64 G8RC:i64:$rA, G8RC:i64:$rB) - Complexity = 3
              // Dst: (MULHD:i64 G8RC:i64:$rA, G8RC:i64:$rB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::MULHU),// ->6488
/*6462*/    OPC_RecordChild0, // #0 = $rA
/*6463*/    OPC_RecordChild1, // #1 = $rB
/*6464*/    OPC_SwitchType /*2 cases */, 9,  MVT::i32,// ->6476
/*6467*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::MULHWU), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (mulhu:i32 GPRC:i32:$rA, GPRC:i32:$rB) - Complexity = 3
              // Dst: (MULHWU:i32 GPRC:i32:$rA, GPRC:i32:$rB)
            /*SwitchType*/ 9,  MVT::i64,// ->6487
/*6478*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::MULHDU), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (mulhu:i64 G8RC:i64:$rA, G8RC:i64:$rB) - Complexity = 3
              // Dst: (MULHDU:i64 G8RC:i64:$rA, G8RC:i64:$rB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(PPCISD::EXTSW_32),// ->6502
/*6491*/    OPC_RecordChild0, // #0 = $rS
/*6492*/    OPC_CheckType, MVT::i32,
/*6494*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::EXTSW_32), 0,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
            // Src: (PPCextsw_32:i32 GPRC:i32:$rS) - Complexity = 3
            // Dst: (EXTSW_32:i32 GPRC:i32:$rS)
          /*SwitchOpcode*/ 13,  TARGET_OPCODE(ISD::SIGN_EXTEND),// ->6518
/*6505*/    OPC_RecordChild0, // #0 = $rS
/*6506*/    OPC_CheckChild0Type, MVT::i32,
/*6508*/    OPC_CheckType, MVT::i64,
/*6510*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::EXTSW_32_64), 0,
                1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
            // Src: (sext:i64 GPRC:i32:$rS) - Complexity = 3
            // Dst: (EXTSW_32_64:i64 GPRC:i32:$rS)
          /*SwitchOpcode*/ 14,  TARGET_OPCODE(ISD::ANY_EXTEND),// ->6535
/*6521*/    OPC_RecordChild0, // #0 = $in
/*6522*/    OPC_CheckChild0Type, MVT::i32,
/*6524*/    OPC_CheckType, MVT::i64,
/*6526*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::OR4To8), 0,
                1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 0, 
            // Src: (anyext:i64 GPRC:i32:$in) - Complexity = 3
            // Dst: (OR4To8:i64 GPRC:i32:$in, GPRC:i32:$in)
          /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TRUNCATE),// ->6550
/*6538*/    OPC_RecordChild0, // #0 = $in
/*6539*/    OPC_CheckType, MVT::i32,
/*6541*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::OR8To4), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 0, 
            // Src: (trunc:i32 G8RC:i64:$in) - Complexity = 3
            // Dst: (OR8To4:i32 G8RC:i64:$in, G8RC:i64:$in)
          /*SwitchOpcode*/ 30,  TARGET_OPCODE(ISD::ZERO_EXTEND),// ->6583
/*6553*/    OPC_RecordChild0, // #0 = $in
/*6554*/    OPC_CheckChild0Type, MVT::i32,
/*6556*/    OPC_CheckType, MVT::i64,
/*6558*/    OPC_EmitNode, TARGET_OPCODE(PPC::OR4To8), 0,
                1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 0,  // Results = #1 
/*6567*/    OPC_EmitInteger, MVT::i32, 0, 
/*6570*/    OPC_EmitInteger, MVT::i32, 32, 
/*6573*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::RLDICL), 0,
                1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 2, 3, 
            // Src: (zext:i64 GPRC:i32:$in) - Complexity = 3
            // Dst: (RLDICL:i64 (OR4To8:i64 GPRC:i32:$in, GPRC:i32:$in), 0:i32, 32:i32)
          /*SwitchOpcode*/ 61|128,1/*189*/,  TARGET_OPCODE(ISD::FNEG),// ->6776
/*6587*/    OPC_Scope, 33|128,1/*161*/, /*->6751*/ // 2 children in Scope
/*6590*/      OPC_MoveChild, 0,
/*6592*/      OPC_SwitchOpcode /*3 cases */, 84,  TARGET_OPCODE(ISD::FADD),// ->6680
/*6596*/        OPC_Scope, 40, /*->6638*/ // 2 children in Scope
/*6598*/          OPC_MoveChild, 0,
/*6600*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::FMUL),
/*6603*/          OPC_RecordChild0, // #0 = $FRA
/*6604*/          OPC_RecordChild1, // #1 = $FRC
/*6605*/          OPC_MoveParent,
/*6606*/          OPC_RecordChild1, // #2 = $FRB
/*6607*/          OPC_MoveParent,
/*6608*/          OPC_SwitchType /*2 cases */, 12,  MVT::f64,// ->6623
/*6611*/            OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6613*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNMADD), 0,
                        1/*#VTs*/, MVT::f64, 3/*#Ops*/, 0, 1, 2, 
                    // Src: (fneg:f64 (fadd:f64 (fmul:f64 F8RC:f64:$FRA, F8RC:f64:$FRC), F8RC:f64:$FRB)) - Complexity = 9
                    // Dst: (FNMADD:f64 F8RC:f64:$FRA, F8RC:f64:$FRC, F8RC:f64:$FRB)
                  /*SwitchType*/ 12,  MVT::f32,// ->6637
/*6625*/            OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6627*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNMADDS), 0,
                        1/*#VTs*/, MVT::f32, 3/*#Ops*/, 0, 1, 2, 
                    // Src: (fneg:f32 (fadd:f32 (fmul:f32 F4RC:f32:$FRA, F4RC:f32:$FRC), F4RC:f32:$FRB)) - Complexity = 9
                    // Dst: (FNMADDS:f32 F4RC:f32:$FRA, F4RC:f32:$FRC, F4RC:f32:$FRB)
                  0, // EndSwitchType
/*6638*/        /*Scope*/ 40, /*->6679*/
/*6639*/          OPC_RecordChild0, // #0 = $FRB
/*6640*/          OPC_MoveChild, 1,
/*6642*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::FMUL),
/*6645*/          OPC_RecordChild0, // #1 = $FRA
/*6646*/          OPC_RecordChild1, // #2 = $FRC
/*6647*/          OPC_MoveParent,
/*6648*/          OPC_MoveParent,
/*6649*/          OPC_SwitchType /*2 cases */, 12,  MVT::f64,// ->6664
/*6652*/            OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6654*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNMADD), 0,
                        1/*#VTs*/, MVT::f64, 3/*#Ops*/, 1, 2, 0, 
                    // Src: (fneg:f64 (fadd:f64 F8RC:f64:$FRB, (fmul:f64 F8RC:f64:$FRA, F8RC:f64:$FRC))) - Complexity = 9
                    // Dst: (FNMADD:f64 F8RC:f64:$FRA, F8RC:f64:$FRC, F8RC:f64:$FRB)
                  /*SwitchType*/ 12,  MVT::f32,// ->6678
/*6666*/            OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6668*/            OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNMADDS), 0,
                        1/*#VTs*/, MVT::f32, 3/*#Ops*/, 1, 2, 0, 
                    // Src: (fneg:f32 (fadd:f32 F4RC:f32:$FRB, (fmul:f32 F4RC:f32:$FRA, F4RC:f32:$FRC))) - Complexity = 9
                    // Dst: (FNMADDS:f32 F4RC:f32:$FRA, F4RC:f32:$FRC, F4RC:f32:$FRB)
                  0, // EndSwitchType
/*6679*/        0, /*End of Scope*/
              /*SwitchOpcode*/ 40,  TARGET_OPCODE(ISD::FSUB),// ->6723
/*6683*/        OPC_MoveChild, 0,
/*6685*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::FMUL),
/*6688*/        OPC_RecordChild0, // #0 = $FRA
/*6689*/        OPC_RecordChild1, // #1 = $FRC
/*6690*/        OPC_MoveParent,
/*6691*/        OPC_RecordChild1, // #2 = $FRB
/*6692*/        OPC_MoveParent,
/*6693*/        OPC_SwitchType /*2 cases */, 12,  MVT::f64,// ->6708
/*6696*/          OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6698*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNMSUB), 0,
                      1/*#VTs*/, MVT::f64, 3/*#Ops*/, 0, 1, 2, 
                  // Src: (fneg:f64 (fsub:f64 (fmul:f64 F8RC:f64:$FRA, F8RC:f64:$FRC), F8RC:f64:$FRB)) - Complexity = 9
                  // Dst: (FNMSUB:f64 F8RC:f64:$FRA, F8RC:f64:$FRC, F8RC:f64:$FRB)
                /*SwitchType*/ 12,  MVT::f32,// ->6722
/*6710*/          OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6712*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNMSUBS), 0,
                      1/*#VTs*/, MVT::f32, 3/*#Ops*/, 0, 1, 2, 
                  // Src: (fneg:f32 (fsub:f32 (fmul:f32 F4RC:f32:$FRA, F4RC:f32:$FRC), F4RC:f32:$FRB)) - Complexity = 9
                  // Dst: (FNMSUBS:f32 F4RC:f32:$FRA, F4RC:f32:$FRC, F4RC:f32:$FRB)
                0, // EndSwitchType
              /*SwitchOpcode*/ 24,  TARGET_OPCODE(ISD::FABS),// ->6750
/*6726*/        OPC_RecordChild0, // #0 = $frB
/*6727*/        OPC_MoveParent,
/*6728*/        OPC_SwitchType /*2 cases */, 8,  MVT::f32,// ->6739
/*6731*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNABSS), 0,
                      1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
                  // Src: (fneg:f32 (fabs:f32 F4RC:f32:$frB)) - Complexity = 6
                  // Dst: (FNABSS:f32 F4RC:f32:$frB)
                /*SwitchType*/ 8,  MVT::f64,// ->6749
/*6741*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNABSD), 0,
                      1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
                  // Src: (fneg:f64 (fabs:f64 F8RC:f64:$frB)) - Complexity = 6
                  // Dst: (FNABSD:f64 F8RC:f64:$frB)
                0, // EndSwitchType
              0, // EndSwitchOpcode
/*6751*/    /*Scope*/ 23, /*->6775*/
/*6752*/      OPC_RecordChild0, // #0 = $frB
/*6753*/      OPC_SwitchType /*2 cases */, 8,  MVT::f32,// ->6764
/*6756*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNEGS), 0,
                    1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
                // Src: (fneg:f32 F4RC:f32:$frB) - Complexity = 3
                // Dst: (FNEGS:f32 F4RC:f32:$frB)
              /*SwitchType*/ 8,  MVT::f64,// ->6774
/*6766*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNEGD), 0,
                    1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
                // Src: (fneg:f64 F8RC:f64:$frB) - Complexity = 3
                // Dst: (FNEGD:f64 F8RC:f64:$frB)
              0, // EndSwitchType
/*6775*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 24,  TARGET_OPCODE(PPCISD::MTFSF),// ->6803
/*6779*/    OPC_CaptureFlagInput,
/*6780*/    OPC_RecordChild0, // #0 = $FM
/*6781*/    OPC_MoveChild, 0,
/*6783*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*6786*/    OPC_CheckType, MVT::i32,
/*6788*/    OPC_MoveParent,
/*6789*/    OPC_RecordChild1, // #1 = $rT
/*6790*/    OPC_RecordChild2, // #2 = $FRB
/*6791*/    OPC_EmitConvertToTarget, 0,
/*6793*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::MTFSF), 0|OPFL_FlagInput,
                1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 1, 2, 
            // Src: (PPCmtfsf:f64 (imm:i32):$FM, F8RC:f64:$rT, F8RC:f64:$FRB) - Complexity = 6
            // Dst: (MTFSF:f64 (imm:i32):$FM, F8RC:f64:$rT, F8RC:f64:$FRB)
          /*SwitchOpcode*/ 47|128,1/*175*/,  TARGET_OPCODE(ISD::FADD),// ->6982
/*6807*/    OPC_Scope, 39, /*->6848*/ // 4 children in Scope
/*6809*/      OPC_MoveChild, 0,
/*6811*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::FMUL),
/*6814*/      OPC_RecordChild0, // #0 = $FRA
/*6815*/      OPC_RecordChild1, // #1 = $FRC
/*6816*/      OPC_MoveParent,
/*6817*/      OPC_RecordChild1, // #2 = $FRB
/*6818*/      OPC_SwitchType /*2 cases */, 12,  MVT::f64,// ->6833
/*6821*/        OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6823*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::FMADD), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 0, 1, 2, 
                // Src: (fadd:f64 (fmul:f64 F8RC:f64:$FRA, F8RC:f64:$FRC), F8RC:f64:$FRB) - Complexity = 6
                // Dst: (FMADD:f64 F8RC:f64:$FRA, F8RC:f64:$FRC, F8RC:f64:$FRB)
              /*SwitchType*/ 12,  MVT::f32,// ->6847
/*6835*/        OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6837*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::FMADDS), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 0, 1, 2, 
                // Src: (fadd:f32 (fmul:f32 F4RC:f32:$FRA, F4RC:f32:$FRC), F4RC:f32:$FRB) - Complexity = 6
                // Dst: (FMADDS:f32 F4RC:f32:$FRA, F4RC:f32:$FRC, F4RC:f32:$FRB)
              0, // EndSwitchType
/*6848*/    /*Scope*/ 68, /*->6917*/
/*6849*/      OPC_RecordChild0, // #0 = $FRB
/*6850*/      OPC_Scope, 38, /*->6890*/ // 2 children in Scope
/*6852*/        OPC_MoveChild, 1,
/*6854*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::FMUL),
/*6857*/        OPC_RecordChild0, // #1 = $FRA
/*6858*/        OPC_RecordChild1, // #2 = $FRC
/*6859*/        OPC_MoveParent,
/*6860*/        OPC_SwitchType /*2 cases */, 12,  MVT::f64,// ->6875
/*6863*/          OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6865*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FMADD), 0,
                      1/*#VTs*/, MVT::f64, 3/*#Ops*/, 1, 2, 0, 
                  // Src: (fadd:f64 F8RC:f64:$FRB, (fmul:f64 F8RC:f64:$FRA, F8RC:f64:$FRC)) - Complexity = 6
                  // Dst: (FMADD:f64 F8RC:f64:$FRA, F8RC:f64:$FRC, F8RC:f64:$FRB)
                /*SwitchType*/ 12,  MVT::f32,// ->6889
/*6877*/          OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6879*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FMADDS), 0,
                      1/*#VTs*/, MVT::f32, 3/*#Ops*/, 1, 2, 0, 
                  // Src: (fadd:f32 F4RC:f32:$FRB, (fmul:f32 F4RC:f32:$FRA, F4RC:f32:$FRC)) - Complexity = 6
                  // Dst: (FMADDS:f32 F4RC:f32:$FRA, F4RC:f32:$FRC, F4RC:f32:$FRB)
                0, // EndSwitchType
/*6890*/      /*Scope*/ 25, /*->6916*/
/*6891*/        OPC_RecordChild1, // #1 = $FRB
/*6892*/        OPC_SwitchType /*2 cases */, 9,  MVT::f64,// ->6904
/*6895*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FADD), 0,
                      1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
                  // Src: (fadd:f64 F8RC:f64:$FRA, F8RC:f64:$FRB) - Complexity = 3
                  // Dst: (FADD:f64 F8RC:f64:$FRA, F8RC:f64:$FRB)
                /*SwitchType*/ 9,  MVT::f32,// ->6915
/*6906*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FADDS), 0,
                      1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
                  // Src: (fadd:f32 F4RC:f32:$FRA, F4RC:f32:$FRB) - Complexity = 3
                  // Dst: (FADDS:f32 F4RC:f32:$FRA, F4RC:f32:$FRB)
                0, // EndSwitchType
/*6916*/      0, /*End of Scope*/
/*6917*/    /*Scope*/ 23, /*->6941*/
/*6918*/      OPC_MoveChild, 0,
/*6920*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::FMUL),
/*6923*/      OPC_RecordChild0, // #0 = $vA
/*6924*/      OPC_RecordChild1, // #1 = $vC
/*6925*/      OPC_MoveParent,
/*6926*/      OPC_RecordChild1, // #2 = $vB
/*6927*/      OPC_CheckType, MVT::v4f32,
/*6929*/      OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6931*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMADDFP), 0,
                  1/*#VTs*/, MVT::v4f32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (fadd:v4f32 (fmul:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vC), VRRC:v4f32:$vB) - Complexity = 6
              // Dst: (VMADDFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vC, VRRC:v4f32:$vB)
/*6941*/    /*Scope*/ 39, /*->6981*/
/*6942*/      OPC_RecordChild0, // #0 = $vB
/*6943*/      OPC_Scope, 22, /*->6967*/ // 2 children in Scope
/*6945*/        OPC_MoveChild, 1,
/*6947*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::FMUL),
/*6950*/        OPC_RecordChild0, // #1 = $vA
/*6951*/        OPC_RecordChild1, // #2 = $vC
/*6952*/        OPC_MoveParent,
/*6953*/        OPC_CheckType, MVT::v4f32,
/*6955*/        OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*6957*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMADDFP), 0,
                    1/*#VTs*/, MVT::v4f32, 3/*#Ops*/, 1, 2, 0, 
                // Src: (fadd:v4f32 VRRC:v4f32:$vB, (fmul:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vC)) - Complexity = 6
                // Dst: (VMADDFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vC, VRRC:v4f32:$vB)
/*6967*/      /*Scope*/ 12, /*->6980*/
/*6968*/        OPC_RecordChild1, // #1 = $vB
/*6969*/        OPC_CheckType, MVT::v4f32,
/*6971*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDFP), 0,
                    1/*#VTs*/, MVT::v4f32, 2/*#Ops*/, 0, 1, 
                // Src: (fadd:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB) - Complexity = 3
                // Dst: (VADDFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*6980*/      0, /*End of Scope*/
/*6981*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 35|128,1/*163*/,  TARGET_OPCODE(ISD::FSUB),// ->7149
/*6986*/    OPC_Scope, 39, /*->7027*/ // 4 children in Scope
/*6988*/      OPC_MoveChild, 0,
/*6990*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::FMUL),
/*6993*/      OPC_RecordChild0, // #0 = $FRA
/*6994*/      OPC_RecordChild1, // #1 = $FRC
/*6995*/      OPC_MoveParent,
/*6996*/      OPC_RecordChild1, // #2 = $FRB
/*6997*/      OPC_SwitchType /*2 cases */, 12,  MVT::f64,// ->7012
/*7000*/        OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*7002*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::FMSUB), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 0, 1, 2, 
                // Src: (fsub:f64 (fmul:f64 F8RC:f64:$FRA, F8RC:f64:$FRC), F8RC:f64:$FRB) - Complexity = 6
                // Dst: (FMSUB:f64 F8RC:f64:$FRA, F8RC:f64:$FRC, F8RC:f64:$FRB)
              /*SwitchType*/ 12,  MVT::f32,// ->7026
/*7014*/        OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*7016*/        OPC_MorphNodeTo, TARGET_OPCODE(PPC::FMSUBS), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 0, 1, 2, 
                // Src: (fsub:f32 (fmul:f32 F4RC:f32:$FRA, F4RC:f32:$FRC), F4RC:f32:$FRB) - Complexity = 6
                // Dst: (FMSUBS:f32 F4RC:f32:$FRA, F4RC:f32:$FRC, F4RC:f32:$FRB)
              0, // EndSwitchType
/*7027*/    /*Scope*/ 68, /*->7096*/
/*7028*/      OPC_RecordChild0, // #0 = $B
/*7029*/      OPC_Scope, 38, /*->7069*/ // 2 children in Scope
/*7031*/        OPC_MoveChild, 1,
/*7033*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::FMUL),
/*7036*/        OPC_RecordChild0, // #1 = $A
/*7037*/        OPC_RecordChild1, // #2 = $C
/*7038*/        OPC_MoveParent,
/*7039*/        OPC_SwitchType /*2 cases */, 12,  MVT::f64,// ->7054
/*7042*/          OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*7044*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNMSUB), 0,
                      1/*#VTs*/, MVT::f64, 3/*#Ops*/, 1, 2, 0, 
                  // Src: (fsub:f64 F8RC:f64:$B, (fmul:f64 F8RC:f64:$A, F8RC:f64:$C)) - Complexity = 6
                  // Dst: (FNMSUB:f64 F8RC:f64:$A, F8RC:f64:$C, F8RC:f64:$B)
                /*SwitchType*/ 12,  MVT::f32,// ->7068
/*7056*/          OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*7058*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FNMSUBS), 0,
                      1/*#VTs*/, MVT::f32, 3/*#Ops*/, 1, 2, 0, 
                  // Src: (fsub:f32 F4RC:f32:$B, (fmul:f32 F4RC:f32:$A, F4RC:f32:$C)) - Complexity = 6
                  // Dst: (FNMSUBS:f32 F4RC:f32:$A, F4RC:f32:$C, F4RC:f32:$B)
                0, // EndSwitchType
/*7069*/      /*Scope*/ 25, /*->7095*/
/*7070*/        OPC_RecordChild1, // #1 = $FRB
/*7071*/        OPC_SwitchType /*2 cases */, 9,  MVT::f64,// ->7083
/*7074*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FSUB), 0,
                      1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
                  // Src: (fsub:f64 F8RC:f64:$FRA, F8RC:f64:$FRB) - Complexity = 3
                  // Dst: (FSUB:f64 F8RC:f64:$FRA, F8RC:f64:$FRB)
                /*SwitchType*/ 9,  MVT::f32,// ->7094
/*7085*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::FSUBS), 0,
                      1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
                  // Src: (fsub:f32 F4RC:f32:$FRA, F4RC:f32:$FRB) - Complexity = 3
                  // Dst: (FSUBS:f32 F4RC:f32:$FRA, F4RC:f32:$FRB)
                0, // EndSwitchType
/*7095*/      0, /*End of Scope*/
/*7096*/    /*Scope*/ 37, /*->7134*/
/*7097*/      OPC_MoveChild, 0,
/*7099*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BUILD_VECTOR),
/*7102*/      OPC_CheckPredicate, 66, // Predicate_V_immneg0
/*7104*/      OPC_MoveParent,
/*7105*/      OPC_MoveChild, 1,
/*7107*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::FSUB),
/*7110*/      OPC_MoveChild, 0,
/*7112*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::FMUL),
/*7115*/      OPC_RecordChild0, // #0 = $vA
/*7116*/      OPC_RecordChild1, // #1 = $vC
/*7117*/      OPC_MoveParent,
/*7118*/      OPC_RecordChild1, // #2 = $vB
/*7119*/      OPC_MoveParent,
/*7120*/      OPC_CheckType, MVT::v4f32,
/*7122*/      OPC_CheckPatternPredicate, 0, // (!NoExcessFPPrecision)
/*7124*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VNMSUBFP), 0,
                  1/*#VTs*/, MVT::v4f32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (fsub:v4f32 (build_vector:v4f32)<<P:Predicate_V_immneg0>>, (fsub:v4f32 (fmul:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vC), VRRC:v4f32:$vB)) - Complexity = 13
              // Dst: (VNMSUBFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vC, VRRC:v4f32:$vB)
/*7134*/    /*Scope*/ 13, /*->7148*/
/*7135*/      OPC_RecordChild0, // #0 = $vA
/*7136*/      OPC_RecordChild1, // #1 = $vB
/*7137*/      OPC_CheckType, MVT::v4f32,
/*7139*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBFP), 0,
                  1/*#VTs*/, MVT::v4f32, 2/*#Ops*/, 0, 1, 
              // Src: (fsub:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB) - Complexity = 3
              // Dst: (VSUBFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*7148*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(PPCISD::FCTIWZ),// ->7163
/*7152*/    OPC_RecordChild0, // #0 = $frB
/*7153*/    OPC_CheckType, MVT::f64,
/*7155*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::FCTIWZ), 0,
                1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
            // Src: (PPCfctiwz:f64 F8RC:f64:$frB) - Complexity = 3
            // Dst: (FCTIWZ:f64 F8RC:f64:$frB)
          /*SwitchOpcode*/ 9,  TARGET_OPCODE(ISD::FP_ROUND),// ->7175
/*7166*/    OPC_RecordChild0, // #0 = $frB
/*7167*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::FRSP), 0,
                1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
            // Src: (fround:f32 F8RC:f64:$frB) - Complexity = 3
            // Dst: (FRSP:f32 F8RC:f64:$frB)
          /*SwitchOpcode*/ 23,  TARGET_OPCODE(ISD::FSQRT),// ->7201
/*7178*/    OPC_RecordChild0, // #0 = $frB
/*7179*/    OPC_SwitchType /*2 cases */, 8,  MVT::f64,// ->7190
/*7182*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::FSQRT), 0,
                  1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
              // Src: (fsqrt:f64 F8RC:f64:$frB) - Complexity = 3
              // Dst: (FSQRT:f64 F8RC:f64:$frB)
            /*SwitchType*/ 8,  MVT::f32,// ->7200
/*7192*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::FSQRTS), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
              // Src: (fsqrt:f32 F4RC:f32:$frB) - Complexity = 3
              // Dst: (FSQRTS:f32 F4RC:f32:$frB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 23,  TARGET_OPCODE(ISD::FABS),// ->7227
/*7204*/    OPC_RecordChild0, // #0 = $frB
/*7205*/    OPC_SwitchType /*2 cases */, 8,  MVT::f32,// ->7216
/*7208*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::FABSS), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
              // Src: (fabs:f32 F4RC:f32:$frB) - Complexity = 3
              // Dst: (FABSS:f32 F4RC:f32:$frB)
            /*SwitchType*/ 8,  MVT::f64,// ->7226
/*7218*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::FABSD), 0,
                  1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
              // Src: (fabs:f64 F8RC:f64:$frB) - Complexity = 3
              // Dst: (FABSD:f64 F8RC:f64:$frB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 7,  TARGET_OPCODE(PPCISD::MFFS),// ->7237
/*7230*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::MFFS), 0|OPFL_FlagOutput,
                1/*#VTs*/, MVT::f64, 0/*#Ops*/, 
            // Src: (PPCmffs:f64) - Complexity = 3
            // Dst: (MFFS:f64)
          /*SwitchOpcode*/ 14,  TARGET_OPCODE(PPCISD::FADDRTZ),// ->7254
/*7240*/    OPC_CaptureFlagInput,
/*7241*/    OPC_RecordChild0, // #0 = $FRA
/*7242*/    OPC_RecordChild1, // #1 = $FRB
/*7243*/    OPC_CheckType, MVT::f64,
/*7245*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::FADDrtz), 0|OPFL_FlagInput|OPFL_FlagOutput,
                1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
            // Src: (PPCfaddrtz:f64 F8RC:f64:$FRA, F8RC:f64:$FRB) - Complexity = 3
            // Dst: (FADDrtz:f64 F8RC:f64:$FRA, F8RC:f64:$FRB)
          /*SwitchOpcode*/ 29,  TARGET_OPCODE(PPCISD::FSEL),// ->7286
/*7257*/    OPC_RecordChild0, // #0 = $FRA
/*7258*/    OPC_RecordChild1, // #1 = $FRC
/*7259*/    OPC_RecordChild2, // #2 = $FRB
/*7260*/    OPC_SwitchType /*2 cases */, 10,  MVT::f64,// ->7273
/*7263*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::FSELD), 0,
                  1/*#VTs*/, MVT::f64, 3/*#Ops*/, 0, 1, 2, 
              // Src: (PPCfsel:f64 F8RC:f64:$FRA, F8RC:f64:$FRC, F8RC:f64:$FRB) - Complexity = 3
              // Dst: (FSELD:f64 F8RC:f64:$FRA, F8RC:f64:$FRC, F8RC:f64:$FRB)
            /*SwitchType*/ 10,  MVT::f32,// ->7285
/*7275*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::FSELS), 0,
                  1/*#VTs*/, MVT::f32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (PPCfsel:f32 F8RC:f64:$FRA, F4RC:f32:$FRC, F4RC:f32:$FRB) - Complexity = 3
              // Dst: (FSELS:f32 F8RC:f64:$FRA, F4RC:f32:$FRC, F4RC:f32:$FRB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::FDIV),// ->7315
/*7289*/    OPC_RecordChild0, // #0 = $FRA
/*7290*/    OPC_RecordChild1, // #1 = $FRB
/*7291*/    OPC_SwitchType /*2 cases */, 9,  MVT::f64,// ->7303
/*7294*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::FDIV), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
              // Src: (fdiv:f64 F8RC:f64:$FRA, F8RC:f64:$FRB) - Complexity = 3
              // Dst: (FDIV:f64 F8RC:f64:$FRA, F8RC:f64:$FRB)
            /*SwitchType*/ 9,  MVT::f32,// ->7314
/*7305*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::FDIVS), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
              // Src: (fdiv:f32 F4RC:f32:$FRA, F4RC:f32:$FRB) - Complexity = 3
              // Dst: (FDIVS:f32 F4RC:f32:$FRA, F4RC:f32:$FRB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 45,  TARGET_OPCODE(ISD::FMUL),// ->7363
/*7318*/    OPC_RecordChild0, // #0 = $FRA
/*7319*/    OPC_RecordChild1, // #1 = $FRB
/*7320*/    OPC_SwitchType /*3 cases */, 9,  MVT::f64,// ->7332
/*7323*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::FMUL), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
              // Src: (fmul:f64 F8RC:f64:$FRA, F8RC:f64:$FRB) - Complexity = 3
              // Dst: (FMUL:f64 F8RC:f64:$FRA, F8RC:f64:$FRB)
            /*SwitchType*/ 9,  MVT::f32,// ->7343
/*7334*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::FMULS), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
              // Src: (fmul:f32 F4RC:f32:$FRA, F4RC:f32:$FRB) - Complexity = 3
              // Dst: (FMULS:f32 F4RC:f32:$FRA, F4RC:f32:$FRB)
            /*SwitchType*/ 17,  MVT::v4f32,// ->7362
/*7345*/      OPC_EmitNode, TARGET_OPCODE(PPC::V_SET0), 0,
                  1/*#VTs*/, MVT::v4i32, 0/*#Ops*/,  // Results = #2 
/*7352*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMADDFP), 0,
                  1/*#VTs*/, MVT::v4f32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (fmul:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB) - Complexity = 3
              // Dst: (VMADDFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB, (V_SET0:v4i32))
            0, // EndSwitchType
          /*SwitchOpcode*/ 13,  TARGET_OPCODE(ISD::FP_EXTEND),// ->7379
/*7366*/    OPC_RecordChild0, // #0 = $src
/*7367*/    OPC_EmitInteger, MVT::i32, PPC::F8RCRegClassID,
/*7370*/    OPC_MorphNodeTo, TARGET_OPCODE(TargetOpcode::COPY_TO_REGCLASS), 0,
                1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
            // Src: (fextend:f64 F4RC:f32:$src) - Complexity = 3
            // Dst: (COPY_TO_REGCLASS:f64 F4RC:f32:$src, F8RC:f64)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(PPCISD::FCFID),// ->7393
/*7382*/    OPC_RecordChild0, // #0 = $frB
/*7383*/    OPC_CheckType, MVT::f64,
/*7385*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::FCFID), 0,
                1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
            // Src: (PPCfcfid:f64 F8RC:f64:$frB) - Complexity = 3
            // Dst: (FCFID:f64 F8RC:f64:$frB)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(PPCISD::FCTIDZ),// ->7407
/*7396*/    OPC_RecordChild0, // #0 = $frB
/*7397*/    OPC_CheckType, MVT::f64,
/*7399*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::FCTIDZ), 0,
                1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
            // Src: (PPCfctidz:f64 F8RC:f64:$frB) - Complexity = 3
            // Dst: (FCTIDZ:f64 F8RC:f64:$frB)
          /*SwitchOpcode*/ 113,  TARGET_OPCODE(ISD::INTRINSIC_W_CHAIN),// ->7523
/*7410*/    OPC_RecordNode,   // #0 = 'intrinsic_w_chain' chained node
/*7411*/    OPC_MoveChild, 1,
/*7413*/    OPC_Scope, 18, /*->7433*/ // 6 children in Scope
/*7415*/      OPC_CheckInteger, 54|128,1/*182*/, 
/*7418*/      OPC_MoveParent,
/*7419*/      OPC_RecordChild2, // #1 = $src
/*7420*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$src #2 #3
/*7423*/      OPC_EmitMergeInputChains1_0,
/*7424*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LVEBX), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_w_chain:v16i8 182:iPTR, xoaddr:iPTR:$src) - Complexity = 17
              // Dst: (LVEBX:v16i8 xoaddr:iPTR:$src)
/*7433*/    /*Scope*/ 18, /*->7452*/
/*7434*/      OPC_CheckInteger, 55|128,1/*183*/, 
/*7437*/      OPC_MoveParent,
/*7438*/      OPC_RecordChild2, // #1 = $src
/*7439*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$src #2 #3
/*7442*/      OPC_EmitMergeInputChains1_0,
/*7443*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LVEHX), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_w_chain:v8i16 183:iPTR, xoaddr:iPTR:$src) - Complexity = 17
              // Dst: (LVEHX:v8i16 xoaddr:iPTR:$src)
/*7452*/    /*Scope*/ 18, /*->7471*/
/*7453*/      OPC_CheckInteger, 56|128,1/*184*/, 
/*7456*/      OPC_MoveParent,
/*7457*/      OPC_RecordChild2, // #1 = $src
/*7458*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$src #2 #3
/*7461*/      OPC_EmitMergeInputChains1_0,
/*7462*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LVEWX), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_w_chain:v4i32 184:iPTR, xoaddr:iPTR:$src) - Complexity = 17
              // Dst: (LVEWX:v4i32 xoaddr:iPTR:$src)
/*7471*/    /*Scope*/ 18, /*->7490*/
/*7472*/      OPC_CheckInteger, 59|128,1/*187*/, 
/*7475*/      OPC_MoveParent,
/*7476*/      OPC_RecordChild2, // #1 = $src
/*7477*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$src #2 #3
/*7480*/      OPC_EmitMergeInputChains1_0,
/*7481*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LVX), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_w_chain:v4i32 187:iPTR, xoaddr:iPTR:$src) - Complexity = 17
              // Dst: (LVX:v4i32 xoaddr:iPTR:$src)
/*7490*/    /*Scope*/ 18, /*->7509*/
/*7491*/      OPC_CheckInteger, 60|128,1/*188*/, 
/*7494*/      OPC_MoveParent,
/*7495*/      OPC_RecordChild2, // #1 = $src
/*7496*/      OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddrIdxOnly:$src #2 #3
/*7499*/      OPC_EmitMergeInputChains1_0,
/*7500*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LVXL), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 2, 3, 
              // Src: (intrinsic_w_chain:v4i32 188:iPTR, xoaddr:iPTR:$src) - Complexity = 17
              // Dst: (LVXL:v4i32 xoaddr:iPTR:$src)
/*7509*/    /*Scope*/ 12, /*->7522*/
/*7510*/      OPC_CheckInteger, 61|128,1/*189*/, 
/*7513*/      OPC_MoveParent,
/*7514*/      OPC_EmitMergeInputChains1_0,
/*7515*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::MFVSCR), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::v8i16, 0/*#Ops*/, 
              // Src: (intrinsic_w_chain:v8i16 189:iPTR) - Complexity = 8
              // Dst: (MFVSCR:v8i16)
/*7522*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 22|128,13/*1686*/,  TARGET_OPCODE(ISD::INTRINSIC_WO_CHAIN),// ->9213
/*7527*/    OPC_MoveChild, 0,
/*7529*/    OPC_Scope, 17, /*->7548*/ // 103 children in Scope
/*7531*/      OPC_CheckInteger, 57|128,1/*185*/, 
/*7534*/      OPC_MoveParent,
/*7535*/      OPC_RecordChild1, // #0 = $src
/*7536*/      OPC_CheckComplexPat, /*CP*/0, /*#*/0, // SelectAddrIdxOnly:$src #1 #2
/*7539*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LVSL), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 1, 2, 
              // Src: (intrinsic_wo_chain:v16i8 185:iPTR, xoaddr:iPTR:$src) - Complexity = 17
              // Dst: (LVSL:v16i8 xoaddr:iPTR:$src)
/*7548*/    /*Scope*/ 17, /*->7566*/
/*7549*/      OPC_CheckInteger, 58|128,1/*186*/, 
/*7552*/      OPC_MoveParent,
/*7553*/      OPC_RecordChild1, // #0 = $src
/*7554*/      OPC_CheckComplexPat, /*CP*/0, /*#*/0, // SelectAddrIdxOnly:$src #1 #2
/*7557*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::LVSR), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 1, 2, 
              // Src: (intrinsic_wo_chain:v16i8 186:iPTR, xoaddr:iPTR:$src) - Complexity = 17
              // Dst: (LVSR:v16i8 xoaddr:iPTR:$src)
/*7566*/    /*Scope*/ 23, /*->7590*/
/*7567*/      OPC_CheckInteger, 109|128,1/*237*/, 
/*7570*/      OPC_MoveParent,
/*7571*/      OPC_RecordChild1, // #0 = $vB
/*7572*/      OPC_RecordChild2, // #1 = $UIMM
/*7573*/      OPC_MoveChild, 2,
/*7575*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*7578*/      OPC_MoveParent,
/*7579*/      OPC_EmitConvertToTarget, 1,
/*7581*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCTSXS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 2, 0, 
              // Src: (intrinsic_wo_chain:v4i32 237:iPTR, VRRC:v4f32:$vB, (imm:i32):$UIMM) - Complexity = 11
              // Dst: (VCTSXS:v4i32 (imm:i32):$UIMM, VRRC:v4f32:$vB)
/*7590*/    /*Scope*/ 23, /*->7614*/
/*7591*/      OPC_CheckInteger, 110|128,1/*238*/, 
/*7594*/      OPC_MoveParent,
/*7595*/      OPC_RecordChild1, // #0 = $vB
/*7596*/      OPC_RecordChild2, // #1 = $UIMM
/*7597*/      OPC_MoveChild, 2,
/*7599*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*7602*/      OPC_MoveParent,
/*7603*/      OPC_EmitConvertToTarget, 1,
/*7605*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCTUXS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 2, 0, 
              // Src: (intrinsic_wo_chain:v4i32 238:iPTR, VRRC:v4f32:$vB, (imm:i32):$UIMM) - Complexity = 11
              // Dst: (VCTUXS:v4i32 (imm:i32):$UIMM, VRRC:v4f32:$vB)
/*7614*/    /*Scope*/ 17, /*->7632*/
/*7615*/      OPC_CheckInteger, 121|128,1/*249*/, 
/*7618*/      OPC_MoveParent,
/*7619*/      OPC_RecordChild1, // #0 = $vA
/*7620*/      OPC_RecordChild2, // #1 = $vB
/*7621*/      OPC_RecordChild3, // #2 = $vC
/*7622*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMHADDSHS), 0,
                  1/*#VTs*/, MVT::v8i16, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v8i16 249:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v8i16:$vC) - Complexity = 8
              // Dst: (VMHADDSHS:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v8i16:$vC)
/*7632*/    /*Scope*/ 17, /*->7650*/
/*7633*/      OPC_CheckInteger, 122|128,1/*250*/, 
/*7636*/      OPC_MoveParent,
/*7637*/      OPC_RecordChild1, // #0 = $vA
/*7638*/      OPC_RecordChild2, // #1 = $vB
/*7639*/      OPC_RecordChild3, // #2 = $vC
/*7640*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMHRADDSHS), 0,
                  1/*#VTs*/, MVT::v8i16, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v8i16 250:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v8i16:$vC) - Complexity = 8
              // Dst: (VMHRADDSHS:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v8i16:$vC)
/*7650*/    /*Scope*/ 17, /*->7668*/
/*7651*/      OPC_CheckInteger, 2|128,2/*258*/, 
/*7654*/      OPC_MoveParent,
/*7655*/      OPC_RecordChild1, // #0 = $vA
/*7656*/      OPC_RecordChild2, // #1 = $vB
/*7657*/      OPC_RecordChild3, // #2 = $vC
/*7658*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMLADDUHM), 0,
                  1/*#VTs*/, MVT::v8i16, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v8i16 258:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v8i16:$vC) - Complexity = 8
              // Dst: (VMLADDUHM:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v8i16:$vC)
/*7668*/    /*Scope*/ 17, /*->7686*/
/*7669*/      OPC_CheckInteger, 18|128,2/*274*/, 
/*7672*/      OPC_MoveParent,
/*7673*/      OPC_RecordChild1, // #0 = $vA
/*7674*/      OPC_RecordChild2, // #1 = $vB
/*7675*/      OPC_RecordChild3, // #2 = $vC
/*7676*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPERM), 0,
                  1/*#VTs*/, MVT::v4i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v4i32 274:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB, VRRC:v16i8:$vC) - Complexity = 8
              // Dst: (VPERM:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB, VRRC:v16i8:$vC)
/*7686*/    /*Scope*/ 17, /*->7704*/
/*7687*/      OPC_CheckInteger, 35|128,2/*291*/, 
/*7690*/      OPC_MoveParent,
/*7691*/      OPC_RecordChild1, // #0 = $vA
/*7692*/      OPC_RecordChild2, // #1 = $vB
/*7693*/      OPC_RecordChild3, // #2 = $vC
/*7694*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSEL), 0,
                  1/*#VTs*/, MVT::v4i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v4i32 291:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB, VRRC:v4i32:$vC) - Complexity = 8
              // Dst: (VSEL:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB, VRRC:v4i32:$vC)
/*7704*/    /*Scope*/ 15, /*->7720*/
/*7705*/      OPC_CheckInteger, 68|128,1/*196*/, 
/*7708*/      OPC_MoveParent,
/*7709*/      OPC_RecordChild1, // #0 = $vA
/*7710*/      OPC_RecordChild2, // #1 = $vB
/*7711*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDCUW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 196:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VADDCUW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*7720*/    /*Scope*/ 15, /*->7736*/
/*7721*/      OPC_CheckInteger, 69|128,1/*197*/, 
/*7724*/      OPC_MoveParent,
/*7725*/      OPC_RecordChild1, // #0 = $vA
/*7726*/      OPC_RecordChild2, // #1 = $vB
/*7727*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDSBS), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 197:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VADDSBS:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*7736*/    /*Scope*/ 15, /*->7752*/
/*7737*/      OPC_CheckInteger, 70|128,1/*198*/, 
/*7740*/      OPC_MoveParent,
/*7741*/      OPC_RecordChild1, // #0 = $vA
/*7742*/      OPC_RecordChild2, // #1 = $vB
/*7743*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDSHS), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 198:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VADDSHS:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*7752*/    /*Scope*/ 15, /*->7768*/
/*7753*/      OPC_CheckInteger, 71|128,1/*199*/, 
/*7756*/      OPC_MoveParent,
/*7757*/      OPC_RecordChild1, // #0 = $vA
/*7758*/      OPC_RecordChild2, // #1 = $vB
/*7759*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDSWS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 199:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VADDSWS:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*7768*/    /*Scope*/ 15, /*->7784*/
/*7769*/      OPC_CheckInteger, 72|128,1/*200*/, 
/*7772*/      OPC_MoveParent,
/*7773*/      OPC_RecordChild1, // #0 = $vA
/*7774*/      OPC_RecordChild2, // #1 = $vB
/*7775*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDUBS), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 200:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VADDUBS:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*7784*/    /*Scope*/ 15, /*->7800*/
/*7785*/      OPC_CheckInteger, 73|128,1/*201*/, 
/*7788*/      OPC_MoveParent,
/*7789*/      OPC_RecordChild1, // #0 = $vA
/*7790*/      OPC_RecordChild2, // #1 = $vB
/*7791*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDUHS), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 201:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VADDUHS:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*7800*/    /*Scope*/ 15, /*->7816*/
/*7801*/      OPC_CheckInteger, 74|128,1/*202*/, 
/*7804*/      OPC_MoveParent,
/*7805*/      OPC_RecordChild1, // #0 = $vA
/*7806*/      OPC_RecordChild2, // #1 = $vB
/*7807*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VADDUWS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 202:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VADDUWS:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*7816*/    /*Scope*/ 15, /*->7832*/
/*7817*/      OPC_CheckInteger, 75|128,1/*203*/, 
/*7820*/      OPC_MoveParent,
/*7821*/      OPC_RecordChild1, // #0 = $vA
/*7822*/      OPC_RecordChild2, // #1 = $vB
/*7823*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VAVGSB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 203:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VAVGSB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*7832*/    /*Scope*/ 15, /*->7848*/
/*7833*/      OPC_CheckInteger, 76|128,1/*204*/, 
/*7836*/      OPC_MoveParent,
/*7837*/      OPC_RecordChild1, // #0 = $vA
/*7838*/      OPC_RecordChild2, // #1 = $vB
/*7839*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VAVGSH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 204:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VAVGSH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*7848*/    /*Scope*/ 15, /*->7864*/
/*7849*/      OPC_CheckInteger, 77|128,1/*205*/, 
/*7852*/      OPC_MoveParent,
/*7853*/      OPC_RecordChild1, // #0 = $vA
/*7854*/      OPC_RecordChild2, // #1 = $vB
/*7855*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VAVGSW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 205:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VAVGSW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*7864*/    /*Scope*/ 15, /*->7880*/
/*7865*/      OPC_CheckInteger, 78|128,1/*206*/, 
/*7868*/      OPC_MoveParent,
/*7869*/      OPC_RecordChild1, // #0 = $vA
/*7870*/      OPC_RecordChild2, // #1 = $vB
/*7871*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VAVGUB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 206:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VAVGUB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*7880*/    /*Scope*/ 15, /*->7896*/
/*7881*/      OPC_CheckInteger, 79|128,1/*207*/, 
/*7884*/      OPC_MoveParent,
/*7885*/      OPC_RecordChild1, // #0 = $vA
/*7886*/      OPC_RecordChild2, // #1 = $vB
/*7887*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VAVGUH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 207:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VAVGUH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*7896*/    /*Scope*/ 15, /*->7912*/
/*7897*/      OPC_CheckInteger, 80|128,1/*208*/, 
/*7900*/      OPC_MoveParent,
/*7901*/      OPC_RecordChild1, // #0 = $vA
/*7902*/      OPC_RecordChild2, // #1 = $vB
/*7903*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VAVGUW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 208:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VAVGUW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*7912*/    /*Scope*/ 15, /*->7928*/
/*7913*/      OPC_CheckInteger, 115|128,1/*243*/, 
/*7916*/      OPC_MoveParent,
/*7917*/      OPC_RecordChild1, // #0 = $vA
/*7918*/      OPC_RecordChild2, // #1 = $vB
/*7919*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMAXSB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 243:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VMAXSB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*7928*/    /*Scope*/ 15, /*->7944*/
/*7929*/      OPC_CheckInteger, 116|128,1/*244*/, 
/*7932*/      OPC_MoveParent,
/*7933*/      OPC_RecordChild1, // #0 = $vA
/*7934*/      OPC_RecordChild2, // #1 = $vB
/*7935*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMAXSH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 244:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VMAXSH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*7944*/    /*Scope*/ 15, /*->7960*/
/*7945*/      OPC_CheckInteger, 117|128,1/*245*/, 
/*7948*/      OPC_MoveParent,
/*7949*/      OPC_RecordChild1, // #0 = $vA
/*7950*/      OPC_RecordChild2, // #1 = $vB
/*7951*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMAXSW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 245:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VMAXSW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*7960*/    /*Scope*/ 15, /*->7976*/
/*7961*/      OPC_CheckInteger, 118|128,1/*246*/, 
/*7964*/      OPC_MoveParent,
/*7965*/      OPC_RecordChild1, // #0 = $vA
/*7966*/      OPC_RecordChild2, // #1 = $vB
/*7967*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMAXUB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 246:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VMAXUB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*7976*/    /*Scope*/ 15, /*->7992*/
/*7977*/      OPC_CheckInteger, 119|128,1/*247*/, 
/*7980*/      OPC_MoveParent,
/*7981*/      OPC_RecordChild1, // #0 = $vA
/*7982*/      OPC_RecordChild2, // #1 = $vB
/*7983*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMAXUH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 247:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VMAXUH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*7992*/    /*Scope*/ 15, /*->8008*/
/*7993*/      OPC_CheckInteger, 120|128,1/*248*/, 
/*7996*/      OPC_MoveParent,
/*7997*/      OPC_RecordChild1, // #0 = $vA
/*7998*/      OPC_RecordChild2, // #1 = $vB
/*7999*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMAXUW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 248:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VMAXUW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8008*/    /*Scope*/ 15, /*->8024*/
/*8009*/      OPC_CheckInteger, 124|128,1/*252*/, 
/*8012*/      OPC_MoveParent,
/*8013*/      OPC_RecordChild1, // #0 = $vA
/*8014*/      OPC_RecordChild2, // #1 = $vB
/*8015*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMINSB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 252:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VMINSB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8024*/    /*Scope*/ 15, /*->8040*/
/*8025*/      OPC_CheckInteger, 125|128,1/*253*/, 
/*8028*/      OPC_MoveParent,
/*8029*/      OPC_RecordChild1, // #0 = $vA
/*8030*/      OPC_RecordChild2, // #1 = $vB
/*8031*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMINSH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 253:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VMINSH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8040*/    /*Scope*/ 15, /*->8056*/
/*8041*/      OPC_CheckInteger, 126|128,1/*254*/, 
/*8044*/      OPC_MoveParent,
/*8045*/      OPC_RecordChild1, // #0 = $vA
/*8046*/      OPC_RecordChild2, // #1 = $vB
/*8047*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMINSW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 254:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VMINSW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8056*/    /*Scope*/ 15, /*->8072*/
/*8057*/      OPC_CheckInteger, 127|128,1/*255*/, 
/*8060*/      OPC_MoveParent,
/*8061*/      OPC_RecordChild1, // #0 = $vA
/*8062*/      OPC_RecordChild2, // #1 = $vB
/*8063*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMINUB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 255:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VMINUB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8072*/    /*Scope*/ 15, /*->8088*/
/*8073*/      OPC_CheckInteger, 0|128,2/*256*/, 
/*8076*/      OPC_MoveParent,
/*8077*/      OPC_RecordChild1, // #0 = $vA
/*8078*/      OPC_RecordChild2, // #1 = $vB
/*8079*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMINUH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 256:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VMINUH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8088*/    /*Scope*/ 15, /*->8104*/
/*8089*/      OPC_CheckInteger, 1|128,2/*257*/, 
/*8092*/      OPC_MoveParent,
/*8093*/      OPC_RecordChild1, // #0 = $vA
/*8094*/      OPC_RecordChild2, // #1 = $vB
/*8095*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMINUW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 257:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VMINUW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8104*/    /*Scope*/ 17, /*->8122*/
/*8105*/      OPC_CheckInteger, 3|128,2/*259*/, 
/*8108*/      OPC_MoveParent,
/*8109*/      OPC_RecordChild1, // #0 = $vA
/*8110*/      OPC_RecordChild2, // #1 = $vB
/*8111*/      OPC_RecordChild3, // #2 = $vC
/*8112*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMSUMMBM), 0,
                  1/*#VTs*/, MVT::v4i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v4i32 259:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB, VRRC:v4i32:$vC) - Complexity = 8
              // Dst: (VMSUMMBM:v4i32 VRRC:v16i8:$vA, VRRC:v16i8:$vB, VRRC:v4i32:$vC)
/*8122*/    /*Scope*/ 17, /*->8140*/
/*8123*/      OPC_CheckInteger, 4|128,2/*260*/, 
/*8126*/      OPC_MoveParent,
/*8127*/      OPC_RecordChild1, // #0 = $vA
/*8128*/      OPC_RecordChild2, // #1 = $vB
/*8129*/      OPC_RecordChild3, // #2 = $vC
/*8130*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMSUMSHM), 0,
                  1/*#VTs*/, MVT::v4i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v4i32 260:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v4i32:$vC) - Complexity = 8
              // Dst: (VMSUMSHM:v4i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v4i32:$vC)
/*8140*/    /*Scope*/ 17, /*->8158*/
/*8141*/      OPC_CheckInteger, 5|128,2/*261*/, 
/*8144*/      OPC_MoveParent,
/*8145*/      OPC_RecordChild1, // #0 = $vA
/*8146*/      OPC_RecordChild2, // #1 = $vB
/*8147*/      OPC_RecordChild3, // #2 = $vC
/*8148*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMSUMSHS), 0,
                  1/*#VTs*/, MVT::v4i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v4i32 261:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v4i32:$vC) - Complexity = 8
              // Dst: (VMSUMSHS:v4i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v4i32:$vC)
/*8158*/    /*Scope*/ 17, /*->8176*/
/*8159*/      OPC_CheckInteger, 6|128,2/*262*/, 
/*8162*/      OPC_MoveParent,
/*8163*/      OPC_RecordChild1, // #0 = $vA
/*8164*/      OPC_RecordChild2, // #1 = $vB
/*8165*/      OPC_RecordChild3, // #2 = $vC
/*8166*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMSUMUBM), 0,
                  1/*#VTs*/, MVT::v4i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v4i32 262:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB, VRRC:v4i32:$vC) - Complexity = 8
              // Dst: (VMSUMUBM:v4i32 VRRC:v16i8:$vA, VRRC:v16i8:$vB, VRRC:v4i32:$vC)
/*8176*/    /*Scope*/ 17, /*->8194*/
/*8177*/      OPC_CheckInteger, 7|128,2/*263*/, 
/*8180*/      OPC_MoveParent,
/*8181*/      OPC_RecordChild1, // #0 = $vA
/*8182*/      OPC_RecordChild2, // #1 = $vB
/*8183*/      OPC_RecordChild3, // #2 = $vC
/*8184*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMSUMUHM), 0,
                  1/*#VTs*/, MVT::v4i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v4i32 263:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v4i32:$vC) - Complexity = 8
              // Dst: (VMSUMUHM:v4i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v4i32:$vC)
/*8194*/    /*Scope*/ 17, /*->8212*/
/*8195*/      OPC_CheckInteger, 8|128,2/*264*/, 
/*8198*/      OPC_MoveParent,
/*8199*/      OPC_RecordChild1, // #0 = $vA
/*8200*/      OPC_RecordChild2, // #1 = $vB
/*8201*/      OPC_RecordChild3, // #2 = $vC
/*8202*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMSUMUHS), 0,
                  1/*#VTs*/, MVT::v4i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v4i32 264:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v4i32:$vC) - Complexity = 8
              // Dst: (VMSUMUHS:v4i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB, VRRC:v4i32:$vC)
/*8212*/    /*Scope*/ 15, /*->8228*/
/*8213*/      OPC_CheckInteger, 9|128,2/*265*/, 
/*8216*/      OPC_MoveParent,
/*8217*/      OPC_RecordChild1, // #0 = $vA
/*8218*/      OPC_RecordChild2, // #1 = $vB
/*8219*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMULESB), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 265:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VMULESB:v8i16 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8228*/    /*Scope*/ 15, /*->8244*/
/*8229*/      OPC_CheckInteger, 10|128,2/*266*/, 
/*8232*/      OPC_MoveParent,
/*8233*/      OPC_RecordChild1, // #0 = $vA
/*8234*/      OPC_RecordChild2, // #1 = $vB
/*8235*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMULESH), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 266:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VMULESH:v4i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8244*/    /*Scope*/ 15, /*->8260*/
/*8245*/      OPC_CheckInteger, 11|128,2/*267*/, 
/*8248*/      OPC_MoveParent,
/*8249*/      OPC_RecordChild1, // #0 = $vA
/*8250*/      OPC_RecordChild2, // #1 = $vB
/*8251*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMULEUB), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 267:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VMULEUB:v8i16 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8260*/    /*Scope*/ 15, /*->8276*/
/*8261*/      OPC_CheckInteger, 12|128,2/*268*/, 
/*8264*/      OPC_MoveParent,
/*8265*/      OPC_RecordChild1, // #0 = $vA
/*8266*/      OPC_RecordChild2, // #1 = $vB
/*8267*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMULEUH), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 268:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VMULEUH:v4i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8276*/    /*Scope*/ 15, /*->8292*/
/*8277*/      OPC_CheckInteger, 13|128,2/*269*/, 
/*8280*/      OPC_MoveParent,
/*8281*/      OPC_RecordChild1, // #0 = $vA
/*8282*/      OPC_RecordChild2, // #1 = $vB
/*8283*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMULOSB), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 269:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VMULOSB:v8i16 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8292*/    /*Scope*/ 15, /*->8308*/
/*8293*/      OPC_CheckInteger, 14|128,2/*270*/, 
/*8296*/      OPC_MoveParent,
/*8297*/      OPC_RecordChild1, // #0 = $vA
/*8298*/      OPC_RecordChild2, // #1 = $vB
/*8299*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMULOSH), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 270:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VMULOSH:v4i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8308*/    /*Scope*/ 15, /*->8324*/
/*8309*/      OPC_CheckInteger, 15|128,2/*271*/, 
/*8312*/      OPC_MoveParent,
/*8313*/      OPC_RecordChild1, // #0 = $vA
/*8314*/      OPC_RecordChild2, // #1 = $vB
/*8315*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMULOUB), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 271:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VMULOUB:v8i16 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8324*/    /*Scope*/ 15, /*->8340*/
/*8325*/      OPC_CheckInteger, 16|128,2/*272*/, 
/*8328*/      OPC_MoveParent,
/*8329*/      OPC_RecordChild1, // #0 = $vA
/*8330*/      OPC_RecordChild2, // #1 = $vB
/*8331*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMULOUH), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 272:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VMULOUH:v4i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8340*/    /*Scope*/ 15, /*->8356*/
/*8341*/      OPC_CheckInteger, 49|128,2/*305*/, 
/*8344*/      OPC_MoveParent,
/*8345*/      OPC_RecordChild1, // #0 = $vA
/*8346*/      OPC_RecordChild2, // #1 = $vB
/*8347*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBCUW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 305:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSUBCUW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8356*/    /*Scope*/ 15, /*->8372*/
/*8357*/      OPC_CheckInteger, 50|128,2/*306*/, 
/*8360*/      OPC_MoveParent,
/*8361*/      OPC_RecordChild1, // #0 = $vA
/*8362*/      OPC_RecordChild2, // #1 = $vB
/*8363*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBSBS), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 306:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VSUBSBS:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8372*/    /*Scope*/ 15, /*->8388*/
/*8373*/      OPC_CheckInteger, 51|128,2/*307*/, 
/*8376*/      OPC_MoveParent,
/*8377*/      OPC_RecordChild1, // #0 = $vA
/*8378*/      OPC_RecordChild2, // #1 = $vB
/*8379*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBSHS), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 307:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VSUBSHS:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8388*/    /*Scope*/ 15, /*->8404*/
/*8389*/      OPC_CheckInteger, 52|128,2/*308*/, 
/*8392*/      OPC_MoveParent,
/*8393*/      OPC_RecordChild1, // #0 = $vA
/*8394*/      OPC_RecordChild2, // #1 = $vB
/*8395*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBSWS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 308:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSUBSWS:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8404*/    /*Scope*/ 15, /*->8420*/
/*8405*/      OPC_CheckInteger, 53|128,2/*309*/, 
/*8408*/      OPC_MoveParent,
/*8409*/      OPC_RecordChild1, // #0 = $vA
/*8410*/      OPC_RecordChild2, // #1 = $vB
/*8411*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBUBS), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 309:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VSUBUBS:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8420*/    /*Scope*/ 15, /*->8436*/
/*8421*/      OPC_CheckInteger, 54|128,2/*310*/, 
/*8424*/      OPC_MoveParent,
/*8425*/      OPC_RecordChild1, // #0 = $vA
/*8426*/      OPC_RecordChild2, // #1 = $vB
/*8427*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBUHS), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 310:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VSUBUHS:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8436*/    /*Scope*/ 15, /*->8452*/
/*8437*/      OPC_CheckInteger, 55|128,2/*311*/, 
/*8440*/      OPC_MoveParent,
/*8441*/      OPC_RecordChild1, // #0 = $vA
/*8442*/      OPC_RecordChild2, // #1 = $vB
/*8443*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUBUWS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 311:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSUBUWS:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8452*/    /*Scope*/ 15, /*->8468*/
/*8453*/      OPC_CheckInteger, 60|128,2/*316*/, 
/*8456*/      OPC_MoveParent,
/*8457*/      OPC_RecordChild1, // #0 = $vA
/*8458*/      OPC_RecordChild2, // #1 = $vB
/*8459*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUMSWS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 316:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSUMSWS:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8468*/    /*Scope*/ 15, /*->8484*/
/*8469*/      OPC_CheckInteger, 56|128,2/*312*/, 
/*8472*/      OPC_MoveParent,
/*8473*/      OPC_RecordChild1, // #0 = $vA
/*8474*/      OPC_RecordChild2, // #1 = $vB
/*8475*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUM2SWS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 312:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSUM2SWS:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8484*/    /*Scope*/ 15, /*->8500*/
/*8485*/      OPC_CheckInteger, 57|128,2/*313*/, 
/*8488*/      OPC_MoveParent,
/*8489*/      OPC_RecordChild1, // #0 = $vA
/*8490*/      OPC_RecordChild2, // #1 = $vB
/*8491*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUM4SBS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 313:iPTR, VRRC:v16i8:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSUM4SBS:v4i32 VRRC:v16i8:$vA, VRRC:v4i32:$vB)
/*8500*/    /*Scope*/ 15, /*->8516*/
/*8501*/      OPC_CheckInteger, 58|128,2/*314*/, 
/*8504*/      OPC_MoveParent,
/*8505*/      OPC_RecordChild1, // #0 = $vA
/*8506*/      OPC_RecordChild2, // #1 = $vB
/*8507*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUM4SHS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 314:iPTR, VRRC:v8i16:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSUM4SHS:v4i32 VRRC:v8i16:$vA, VRRC:v4i32:$vB)
/*8516*/    /*Scope*/ 15, /*->8532*/
/*8517*/      OPC_CheckInteger, 59|128,2/*315*/, 
/*8520*/      OPC_MoveParent,
/*8521*/      OPC_RecordChild1, // #0 = $vA
/*8522*/      OPC_RecordChild2, // #1 = $vB
/*8523*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSUM4UBS), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 315:iPTR, VRRC:v16i8:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSUM4UBS:v4i32 VRRC:v16i8:$vA, VRRC:v4i32:$vB)
/*8532*/    /*Scope*/ 15, /*->8548*/
/*8533*/      OPC_CheckInteger, 31|128,2/*287*/, 
/*8536*/      OPC_MoveParent,
/*8537*/      OPC_RecordChild1, // #0 = $vA
/*8538*/      OPC_RecordChild2, // #1 = $vB
/*8539*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VRLB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 287:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VRLB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8548*/    /*Scope*/ 15, /*->8564*/
/*8549*/      OPC_CheckInteger, 32|128,2/*288*/, 
/*8552*/      OPC_MoveParent,
/*8553*/      OPC_RecordChild1, // #0 = $vA
/*8554*/      OPC_RecordChild2, // #1 = $vB
/*8555*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VRLH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 288:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VRLH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8564*/    /*Scope*/ 15, /*->8580*/
/*8565*/      OPC_CheckInteger, 33|128,2/*289*/, 
/*8568*/      OPC_MoveParent,
/*8569*/      OPC_RecordChild1, // #0 = $vA
/*8570*/      OPC_RecordChild2, // #1 = $vB
/*8571*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VRLW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 289:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VRLW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8580*/    /*Scope*/ 15, /*->8596*/
/*8581*/      OPC_CheckInteger, 36|128,2/*292*/, 
/*8584*/      OPC_MoveParent,
/*8585*/      OPC_RecordChild1, // #0 = $vA
/*8586*/      OPC_RecordChild2, // #1 = $vB
/*8587*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSL), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 292:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSL:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8596*/    /*Scope*/ 15, /*->8612*/
/*8597*/      OPC_CheckInteger, 39|128,2/*295*/, 
/*8600*/      OPC_MoveParent,
/*8601*/      OPC_RecordChild1, // #0 = $vA
/*8602*/      OPC_RecordChild2, // #1 = $vB
/*8603*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSLO), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 295:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSLO:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8612*/    /*Scope*/ 15, /*->8628*/
/*8613*/      OPC_CheckInteger, 37|128,2/*293*/, 
/*8616*/      OPC_MoveParent,
/*8617*/      OPC_RecordChild1, // #0 = $vA
/*8618*/      OPC_RecordChild2, // #1 = $vB
/*8619*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSLB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 293:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VSLB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8628*/    /*Scope*/ 15, /*->8644*/
/*8629*/      OPC_CheckInteger, 38|128,2/*294*/, 
/*8632*/      OPC_MoveParent,
/*8633*/      OPC_RecordChild1, // #0 = $vA
/*8634*/      OPC_RecordChild2, // #1 = $vB
/*8635*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSLH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 294:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VSLH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8644*/    /*Scope*/ 15, /*->8660*/
/*8645*/      OPC_CheckInteger, 40|128,2/*296*/, 
/*8648*/      OPC_MoveParent,
/*8649*/      OPC_RecordChild1, // #0 = $vA
/*8650*/      OPC_RecordChild2, // #1 = $vB
/*8651*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSLW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 296:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSLW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8660*/    /*Scope*/ 15, /*->8676*/
/*8661*/      OPC_CheckInteger, 41|128,2/*297*/, 
/*8664*/      OPC_MoveParent,
/*8665*/      OPC_RecordChild1, // #0 = $vA
/*8666*/      OPC_RecordChild2, // #1 = $vB
/*8667*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSR), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 297:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSR:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8676*/    /*Scope*/ 15, /*->8692*/
/*8677*/      OPC_CheckInteger, 47|128,2/*303*/, 
/*8680*/      OPC_MoveParent,
/*8681*/      OPC_RecordChild1, // #0 = $vA
/*8682*/      OPC_RecordChild2, // #1 = $vB
/*8683*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRO), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 303:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSRO:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8692*/    /*Scope*/ 15, /*->8708*/
/*8693*/      OPC_CheckInteger, 42|128,2/*298*/, 
/*8696*/      OPC_MoveParent,
/*8697*/      OPC_RecordChild1, // #0 = $vA
/*8698*/      OPC_RecordChild2, // #1 = $vB
/*8699*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRAB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 298:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VSRAB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8708*/    /*Scope*/ 15, /*->8724*/
/*8709*/      OPC_CheckInteger, 43|128,2/*299*/, 
/*8712*/      OPC_MoveParent,
/*8713*/      OPC_RecordChild1, // #0 = $vA
/*8714*/      OPC_RecordChild2, // #1 = $vB
/*8715*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRAH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 299:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VSRAH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8724*/    /*Scope*/ 15, /*->8740*/
/*8725*/      OPC_CheckInteger, 44|128,2/*300*/, 
/*8728*/      OPC_MoveParent,
/*8729*/      OPC_RecordChild1, // #0 = $vA
/*8730*/      OPC_RecordChild2, // #1 = $vB
/*8731*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRAW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 300:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSRAW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8740*/    /*Scope*/ 15, /*->8756*/
/*8741*/      OPC_CheckInteger, 45|128,2/*301*/, 
/*8744*/      OPC_MoveParent,
/*8745*/      OPC_RecordChild1, // #0 = $vA
/*8746*/      OPC_RecordChild2, // #1 = $vB
/*8747*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 301:iPTR, VRRC:v16i8:$vA, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VSRB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*8756*/    /*Scope*/ 15, /*->8772*/
/*8757*/      OPC_CheckInteger, 46|128,2/*302*/, 
/*8760*/      OPC_MoveParent,
/*8761*/      OPC_RecordChild1, // #0 = $vA
/*8762*/      OPC_RecordChild2, // #1 = $vB
/*8763*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 302:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VSRH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8772*/    /*Scope*/ 15, /*->8788*/
/*8773*/      OPC_CheckInteger, 48|128,2/*304*/, 
/*8776*/      OPC_MoveParent,
/*8777*/      OPC_RecordChild1, // #0 = $vA
/*8778*/      OPC_RecordChild2, // #1 = $vB
/*8779*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSRW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4i32 304:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VSRW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8788*/    /*Scope*/ 15, /*->8804*/
/*8789*/      OPC_CheckInteger, 19|128,2/*275*/, 
/*8792*/      OPC_MoveParent,
/*8793*/      OPC_RecordChild1, // #0 = $vA
/*8794*/      OPC_RecordChild2, // #1 = $vB
/*8795*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKPX), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 275:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VPKPX:v8i16 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8804*/    /*Scope*/ 15, /*->8820*/
/*8805*/      OPC_CheckInteger, 20|128,2/*276*/, 
/*8808*/      OPC_MoveParent,
/*8809*/      OPC_RecordChild1, // #0 = $vA
/*8810*/      OPC_RecordChild2, // #1 = $vB
/*8811*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKSHSS), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 276:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VPKSHSS:v16i8 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8820*/    /*Scope*/ 15, /*->8836*/
/*8821*/      OPC_CheckInteger, 21|128,2/*277*/, 
/*8824*/      OPC_MoveParent,
/*8825*/      OPC_RecordChild1, // #0 = $vA
/*8826*/      OPC_RecordChild2, // #1 = $vB
/*8827*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKSHUS), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 277:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VPKSHUS:v16i8 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8836*/    /*Scope*/ 15, /*->8852*/
/*8837*/      OPC_CheckInteger, 22|128,2/*278*/, 
/*8840*/      OPC_MoveParent,
/*8841*/      OPC_RecordChild1, // #0 = $vA
/*8842*/      OPC_RecordChild2, // #1 = $vB
/*8843*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKSWSS), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 278:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VPKSWSS:v16i8 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8852*/    /*Scope*/ 15, /*->8868*/
/*8853*/      OPC_CheckInteger, 23|128,2/*279*/, 
/*8856*/      OPC_MoveParent,
/*8857*/      OPC_RecordChild1, // #0 = $vA
/*8858*/      OPC_RecordChild2, // #1 = $vB
/*8859*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKSWUS), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 279:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VPKSWUS:v8i16 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8868*/    /*Scope*/ 15, /*->8884*/
/*8869*/      OPC_CheckInteger, 24|128,2/*280*/, 
/*8872*/      OPC_MoveParent,
/*8873*/      OPC_RecordChild1, // #0 = $vA
/*8874*/      OPC_RecordChild2, // #1 = $vB
/*8875*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKUHUS), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v16i8 280:iPTR, VRRC:v8i16:$vA, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VPKUHUS:v16i8 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*8884*/    /*Scope*/ 15, /*->8900*/
/*8885*/      OPC_CheckInteger, 25|128,2/*281*/, 
/*8888*/      OPC_MoveParent,
/*8889*/      OPC_RecordChild1, // #0 = $vA
/*8890*/      OPC_RecordChild2, // #1 = $vB
/*8891*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKUWUS), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v8i16 281:iPTR, VRRC:v4i32:$vA, VRRC:v4i32:$vB) - Complexity = 8
              // Dst: (VPKUWUS:v8i16 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*8900*/    /*Scope*/ 13, /*->8914*/
/*8901*/      OPC_CheckInteger, 61|128,2/*317*/, 
/*8904*/      OPC_MoveParent,
/*8905*/      OPC_RecordChild1, // #0 = $vB
/*8906*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VUPKHPX), 0,
                  1/*#VTs*/, MVT::v4i32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4i32 317:iPTR, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VUPKHPX:v4i32 VRRC:v8i16:$vB)
/*8914*/    /*Scope*/ 13, /*->8928*/
/*8915*/      OPC_CheckInteger, 62|128,2/*318*/, 
/*8918*/      OPC_MoveParent,
/*8919*/      OPC_RecordChild1, // #0 = $vB
/*8920*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VUPKHSB), 0,
                  1/*#VTs*/, MVT::v8i16, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v8i16 318:iPTR, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VUPKHSB:v8i16 VRRC:v16i8:$vB)
/*8928*/    /*Scope*/ 13, /*->8942*/
/*8929*/      OPC_CheckInteger, 63|128,2/*319*/, 
/*8932*/      OPC_MoveParent,
/*8933*/      OPC_RecordChild1, // #0 = $vB
/*8934*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VUPKHSH), 0,
                  1/*#VTs*/, MVT::v4i32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4i32 319:iPTR, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VUPKHSH:v4i32 VRRC:v8i16:$vB)
/*8942*/    /*Scope*/ 13, /*->8956*/
/*8943*/      OPC_CheckInteger, 64|128,2/*320*/, 
/*8946*/      OPC_MoveParent,
/*8947*/      OPC_RecordChild1, // #0 = $vB
/*8948*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VUPKLPX), 0,
                  1/*#VTs*/, MVT::v4i32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4i32 320:iPTR, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VUPKLPX:v4i32 VRRC:v8i16:$vB)
/*8956*/    /*Scope*/ 13, /*->8970*/
/*8957*/      OPC_CheckInteger, 65|128,2/*321*/, 
/*8960*/      OPC_MoveParent,
/*8961*/      OPC_RecordChild1, // #0 = $vB
/*8962*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VUPKLSB), 0,
                  1/*#VTs*/, MVT::v8i16, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v8i16 321:iPTR, VRRC:v16i8:$vB) - Complexity = 8
              // Dst: (VUPKLSB:v8i16 VRRC:v16i8:$vB)
/*8970*/    /*Scope*/ 13, /*->8984*/
/*8971*/      OPC_CheckInteger, 66|128,2/*322*/, 
/*8974*/      OPC_MoveParent,
/*8975*/      OPC_RecordChild1, // #0 = $vB
/*8976*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VUPKLSH), 0,
                  1/*#VTs*/, MVT::v4i32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4i32 322:iPTR, VRRC:v8i16:$vB) - Complexity = 8
              // Dst: (VUPKLSH:v4i32 VRRC:v8i16:$vB)
/*8984*/    /*Scope*/ 23, /*->9008*/
/*8985*/      OPC_CheckInteger, 81|128,1/*209*/, 
/*8988*/      OPC_MoveParent,
/*8989*/      OPC_RecordChild1, // #0 = $vB
/*8990*/      OPC_RecordChild2, // #1 = $UIMM
/*8991*/      OPC_MoveChild, 2,
/*8993*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*8996*/      OPC_MoveParent,
/*8997*/      OPC_EmitConvertToTarget, 1,
/*8999*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCFSX), 0,
                  1/*#VTs*/, MVT::v4f32, 2/*#Ops*/, 2, 0, 
              // Src: (intrinsic_wo_chain:v4f32 209:iPTR, VRRC:v4i32:$vB, (imm:i32):$UIMM) - Complexity = 11
              // Dst: (VCFSX:v4f32 (imm:i32):$UIMM, VRRC:v4i32:$vB)
/*9008*/    /*Scope*/ 23, /*->9032*/
/*9009*/      OPC_CheckInteger, 82|128,1/*210*/, 
/*9012*/      OPC_MoveParent,
/*9013*/      OPC_RecordChild1, // #0 = $vB
/*9014*/      OPC_RecordChild2, // #1 = $UIMM
/*9015*/      OPC_MoveChild, 2,
/*9017*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*9020*/      OPC_MoveParent,
/*9021*/      OPC_EmitConvertToTarget, 1,
/*9023*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCFUX), 0,
                  1/*#VTs*/, MVT::v4f32, 2/*#Ops*/, 2, 0, 
              // Src: (intrinsic_wo_chain:v4f32 210:iPTR, VRRC:v4i32:$vB, (imm:i32):$UIMM) - Complexity = 11
              // Dst: (VCFUX:v4f32 (imm:i32):$UIMM, VRRC:v4i32:$vB)
/*9032*/    /*Scope*/ 13, /*->9046*/
/*9033*/      OPC_CheckInteger, 111|128,1/*239*/, 
/*9036*/      OPC_MoveParent,
/*9037*/      OPC_RecordChild1, // #0 = $vB
/*9038*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VEXPTEFP), 0,
                  1/*#VTs*/, MVT::v4f32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4f32 239:iPTR, VRRC:v4f32:$vB) - Complexity = 8
              // Dst: (VEXPTEFP:v4f32 VRRC:v4f32:$vB)
/*9046*/    /*Scope*/ 13, /*->9060*/
/*9047*/      OPC_CheckInteger, 112|128,1/*240*/, 
/*9050*/      OPC_MoveParent,
/*9051*/      OPC_RecordChild1, // #0 = $vB
/*9052*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VLOGEFP), 0,
                  1/*#VTs*/, MVT::v4f32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4f32 240:iPTR, VRRC:v4f32:$vB) - Complexity = 8
              // Dst: (VLOGEFP:v4f32 VRRC:v4f32:$vB)
/*9060*/    /*Scope*/ 15, /*->9076*/
/*9061*/      OPC_CheckInteger, 114|128,1/*242*/, 
/*9064*/      OPC_MoveParent,
/*9065*/      OPC_RecordChild1, // #0 = $vA
/*9066*/      OPC_RecordChild2, // #1 = $vB
/*9067*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMAXFP), 0,
                  1/*#VTs*/, MVT::v4f32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4f32 242:iPTR, VRRC:v4f32:$vA, VRRC:v4f32:$vB) - Complexity = 8
              // Dst: (VMAXFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*9076*/    /*Scope*/ 15, /*->9092*/
/*9077*/      OPC_CheckInteger, 123|128,1/*251*/, 
/*9080*/      OPC_MoveParent,
/*9081*/      OPC_RecordChild1, // #0 = $vA
/*9082*/      OPC_RecordChild2, // #1 = $vB
/*9083*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMINFP), 0,
                  1/*#VTs*/, MVT::v4f32, 2/*#Ops*/, 0, 1, 
              // Src: (intrinsic_wo_chain:v4f32 251:iPTR, VRRC:v4f32:$vA, VRRC:v4f32:$vB) - Complexity = 8
              // Dst: (VMINFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*9092*/    /*Scope*/ 13, /*->9106*/
/*9093*/      OPC_CheckInteger, 26|128,2/*282*/, 
/*9096*/      OPC_MoveParent,
/*9097*/      OPC_RecordChild1, // #0 = $vB
/*9098*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VREFP), 0,
                  1/*#VTs*/, MVT::v4f32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4f32 282:iPTR, VRRC:v4f32:$vB) - Complexity = 8
              // Dst: (VREFP:v4f32 VRRC:v4f32:$vB)
/*9106*/    /*Scope*/ 13, /*->9120*/
/*9107*/      OPC_CheckInteger, 27|128,2/*283*/, 
/*9110*/      OPC_MoveParent,
/*9111*/      OPC_RecordChild1, // #0 = $vB
/*9112*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VRFIM), 0,
                  1/*#VTs*/, MVT::v4f32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4f32 283:iPTR, VRRC:v4f32:$vB) - Complexity = 8
              // Dst: (VRFIM:v4f32 VRRC:v4f32:$vB)
/*9120*/    /*Scope*/ 13, /*->9134*/
/*9121*/      OPC_CheckInteger, 28|128,2/*284*/, 
/*9124*/      OPC_MoveParent,
/*9125*/      OPC_RecordChild1, // #0 = $vB
/*9126*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VRFIN), 0,
                  1/*#VTs*/, MVT::v4f32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4f32 284:iPTR, VRRC:v4f32:$vB) - Complexity = 8
              // Dst: (VRFIN:v4f32 VRRC:v4f32:$vB)
/*9134*/    /*Scope*/ 13, /*->9148*/
/*9135*/      OPC_CheckInteger, 29|128,2/*285*/, 
/*9138*/      OPC_MoveParent,
/*9139*/      OPC_RecordChild1, // #0 = $vB
/*9140*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VRFIP), 0,
                  1/*#VTs*/, MVT::v4f32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4f32 285:iPTR, VRRC:v4f32:$vB) - Complexity = 8
              // Dst: (VRFIP:v4f32 VRRC:v4f32:$vB)
/*9148*/    /*Scope*/ 13, /*->9162*/
/*9149*/      OPC_CheckInteger, 30|128,2/*286*/, 
/*9152*/      OPC_MoveParent,
/*9153*/      OPC_RecordChild1, // #0 = $vB
/*9154*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VRFIZ), 0,
                  1/*#VTs*/, MVT::v4f32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4f32 286:iPTR, VRRC:v4f32:$vB) - Complexity = 8
              // Dst: (VRFIZ:v4f32 VRRC:v4f32:$vB)
/*9162*/    /*Scope*/ 13, /*->9176*/
/*9163*/      OPC_CheckInteger, 34|128,2/*290*/, 
/*9166*/      OPC_MoveParent,
/*9167*/      OPC_RecordChild1, // #0 = $vB
/*9168*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VRSQRTEFP), 0,
                  1/*#VTs*/, MVT::v4f32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:v4f32 290:iPTR, VRRC:v4f32:$vB) - Complexity = 8
              // Dst: (VRSQRTEFP:v4f32 VRRC:v4f32:$vB)
/*9176*/    /*Scope*/ 17, /*->9194*/
/*9177*/      OPC_CheckInteger, 113|128,1/*241*/, 
/*9180*/      OPC_MoveParent,
/*9181*/      OPC_RecordChild1, // #0 = $A
/*9182*/      OPC_RecordChild2, // #1 = $B
/*9183*/      OPC_RecordChild3, // #2 = $C
/*9184*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMADDFP), 0,
                  1/*#VTs*/, MVT::v4f32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v4f32 241:iPTR, VRRC:v4f32:$A, VRRC:v4f32:$B, VRRC:v4f32:$C) - Complexity = 8
              // Dst: (VMADDFP:v4f32 VRRC:v4f32:$A, VRRC:v4f32:$B, VRRC:v4f32:$C)
/*9194*/    /*Scope*/ 17, /*->9212*/
/*9195*/      OPC_CheckInteger, 17|128,2/*273*/, 
/*9198*/      OPC_MoveParent,
/*9199*/      OPC_RecordChild1, // #0 = $A
/*9200*/      OPC_RecordChild2, // #1 = $B
/*9201*/      OPC_RecordChild3, // #2 = $C
/*9202*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VNMSUBFP), 0,
                  1/*#VTs*/, MVT::v4f32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (intrinsic_wo_chain:v4f32 273:iPTR, VRRC:v4f32:$A, VRRC:v4f32:$B, VRRC:v4f32:$C) - Complexity = 8
              // Dst: (VNMSUBFP:v4f32 VRRC:v4f32:$A, VRRC:v4f32:$B, VRRC:v4f32:$C)
/*9212*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 40,  TARGET_OPCODE(PPCISD::STCX),// ->9256
/*9216*/    OPC_RecordNode,   // #0 = 'PPCstcx' chained node
/*9217*/    OPC_RecordChild1, // #1 = $rS
/*9218*/    OPC_Scope, 17, /*->9237*/ // 2 children in Scope
/*9220*/      OPC_CheckChild1Type, MVT::i32,
/*9222*/      OPC_RecordChild2, // #2 = $dst
/*9223*/      OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*9226*/      OPC_EmitMergeInputChains1_0,
/*9227*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::STWCX), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::i32, 3/*#Ops*/, 1, 3, 4, 
              // Src: (PPCstcx GPRC:i32:$rS, xoaddr:iPTR:$dst) - Complexity = 12
              // Dst: (STWCX:i32 GPRC:i32:$rS, xoaddr:iPTR:$dst)
/*9237*/    /*Scope*/ 17, /*->9255*/
/*9238*/      OPC_CheckChild1Type, MVT::i64,
/*9240*/      OPC_RecordChild2, // #2 = $dst
/*9241*/      OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*9244*/      OPC_EmitMergeInputChains1_0,
/*9245*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::STDCX), 0|OPFL_Chain,
                  1/*#VTs*/, MVT::i32, 3/*#Ops*/, 1, 3, 4, 
              // Src: (PPCstcx G8RC:i64:$rS, xoaddr:iPTR:$dst) - Complexity = 12
              // Dst: (STDCX:i32 G8RC:i64:$rS, xoaddr:iPTR:$dst)
/*9255*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 41,  TARGET_OPCODE(PPCISD::STBRX),// ->9300
/*9259*/    OPC_RecordNode,   // #0 = 'PPCstbrx' chained node
/*9260*/    OPC_RecordChild1, // #1 = $rS
/*9261*/    OPC_RecordChild2, // #2 = $dst
/*9262*/    OPC_MoveChild, 3,
/*9264*/    OPC_Scope, 16, /*->9282*/ // 2 children in Scope
/*9266*/      OPC_CheckValueType, MVT::i16,
/*9268*/      OPC_MoveParent,
/*9269*/      OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*9272*/      OPC_EmitMergeInputChains1_0,
/*9273*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::STHBRX), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
              // Src: (PPCstbrx GPRC:i32:$rS, xoaddr:iPTR:$dst, i16:Other) - Complexity = 12
              // Dst: (STHBRX GPRC:i32:$rS, xoaddr:iPTR:$dst)
/*9282*/    /*Scope*/ 16, /*->9299*/
/*9283*/      OPC_CheckValueType, MVT::i32,
/*9285*/      OPC_MoveParent,
/*9286*/      OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*9289*/      OPC_EmitMergeInputChains1_0,
/*9290*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::STWBRX), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
              // Src: (PPCstbrx GPRC:i32:$rS, xoaddr:iPTR:$dst, i32:Other) - Complexity = 12
              // Dst: (STWBRX GPRC:i32:$rS, xoaddr:iPTR:$dst)
/*9299*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 16,  TARGET_OPCODE(PPCISD::STFIWX),// ->9319
/*9303*/    OPC_RecordNode,   // #0 = 'PPCstfiwx' chained node
/*9304*/    OPC_RecordChild1, // #1 = $frS
/*9305*/    OPC_RecordChild2, // #2 = $dst
/*9306*/    OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddrIdxOnly:$dst #3 #4
/*9309*/    OPC_EmitMergeInputChains1_0,
/*9310*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::STFIWX), 0|OPFL_Chain,
                0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
            // Src: (PPCstfiwx F8RC:f64:$frS, xoaddr:iPTR:$dst) - Complexity = 12
            // Dst: (STFIWX F8RC:f64:$frS, xoaddr:iPTR:$dst)
          /*SwitchOpcode*/ 35,  TARGET_OPCODE(PPCISD::STD_32),// ->9357
/*9322*/    OPC_RecordNode,   // #0 = 'PPCstd_32' chained node
/*9323*/    OPC_RecordChild1, // #1 = $rT
/*9324*/    OPC_CheckChild1Type, MVT::i32,
/*9326*/    OPC_RecordChild2, // #2 = $dst
/*9327*/    OPC_Scope, 13, /*->9342*/ // 2 children in Scope
/*9329*/      OPC_CheckComplexPat, /*CP*/3, /*#*/2, // SelectAddrImmShift:$dst #3 #4
/*9332*/      OPC_EmitMergeInputChains1_0,
/*9333*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::STD_32), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
              // Src: (PPCstd_32 GPRC:i32:$rT, ixaddr:iPTR:$dst) - Complexity = 12
              // Dst: (STD_32 GPRC:i32:$rT, ixaddr:iPTR:$dst)
/*9342*/    /*Scope*/ 13, /*->9356*/
/*9343*/      OPC_CheckComplexPat, /*CP*/2, /*#*/2, // SelectAddrIdx:$dst #3 #4
/*9346*/      OPC_EmitMergeInputChains1_0,
/*9347*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::STDX_32), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
              // Src: (PPCstd_32 GPRC:i32:$rT, xaddr:iPTR:$dst) - Complexity = 12
              // Dst: (STDX_32 GPRC:i32:$rT, xaddr:iPTR:$dst)
/*9356*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::CALLSEQ_END),// ->9386
/*9360*/    OPC_RecordNode,   // #0 = 'callseq_end' chained node
/*9361*/    OPC_CaptureFlagInput,
/*9362*/    OPC_RecordChild1, // #1 = $amt1
/*9363*/    OPC_MoveChild, 1,
/*9365*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*9368*/    OPC_MoveParent,
/*9369*/    OPC_RecordChild2, // #2 = $amt2
/*9370*/    OPC_MoveChild, 2,
/*9372*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*9375*/    OPC_MoveParent,
/*9376*/    OPC_EmitMergeInputChains1_0,
/*9377*/    OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADJCALLSTACKUP), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
            // Src: (callseq_end (timm:i32):$amt1, (timm:i32):$amt2) - Complexity = 9
            // Dst: (ADJCALLSTACKUP:i32 (timm:i32):$amt1, (timm:i32):$amt2)
          /*SwitchOpcode*/ 70|128,1/*198*/,  TARGET_OPCODE(PPCISD::TC_RETURN),// ->9588
/*9390*/    OPC_RecordNode,   // #0 = 'PPCtc_return' chained node
/*9391*/    OPC_CaptureFlagInput,
/*9392*/    OPC_RecordChild1, // #1 = $func
/*9393*/    OPC_Scope, 21|128,1/*149*/, /*->9545*/ // 3 children in Scope
/*9396*/      OPC_MoveChild, 1,
/*9398*/      OPC_SwitchOpcode /*3 cases */, 48,  TARGET_OPCODE(ISD::Constant),// ->9450
/*9402*/        OPC_SwitchType /*2 cases */, 21,  MVT::i32,// ->9426
/*9405*/          OPC_MoveParent,
/*9406*/          OPC_RecordChild2, // #2 = $offset
/*9407*/          OPC_MoveChild, 2,
/*9409*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*9412*/          OPC_MoveParent,
/*9413*/          OPC_EmitMergeInputChains1_0,
/*9414*/          OPC_EmitConvertToTarget, 1,
/*9416*/          OPC_EmitConvertToTarget, 2,
/*9418*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::TCRETURNai), 0|OPFL_Chain|OPFL_FlagInput|OPFL_Variadic2,
                      0/*#VTs*/, 2/*#Ops*/, 3, 4, 
                  // Src: (PPCtc_return (imm:i32):$func, (imm:i32):$offset) - Complexity = 9
                  // Dst: (TCRETURNai (imm:i32):$func, (imm:i32):$offset)
                /*SwitchType*/ 21,  MVT::i64,// ->9449
/*9428*/          OPC_MoveParent,
/*9429*/          OPC_RecordChild2, // #2 = $offset
/*9430*/          OPC_MoveChild, 2,
/*9432*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*9435*/          OPC_MoveParent,
/*9436*/          OPC_EmitMergeInputChains1_0,
/*9437*/          OPC_EmitConvertToTarget, 1,
/*9439*/          OPC_EmitConvertToTarget, 2,
/*9441*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::TCRETURNai8), 0|OPFL_Chain|OPFL_FlagInput|OPFL_Variadic2,
                      0/*#VTs*/, 2/*#Ops*/, 3, 4, 
                  // Src: (PPCtc_return (imm:i64):$func, (imm:i32):$offset) - Complexity = 9
                  // Dst: (TCRETURNai8 (imm:i64):$func, (imm:i32):$offset)
                0, // EndSwitchType
              /*SwitchOpcode*/ 44,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->9497
/*9453*/        OPC_SwitchType /*2 cases */, 19,  MVT::i32,// ->9475
/*9456*/          OPC_MoveParent,
/*9457*/          OPC_RecordChild2, // #2 = $imm
/*9458*/          OPC_MoveChild, 2,
/*9460*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*9463*/          OPC_MoveParent,
/*9464*/          OPC_EmitMergeInputChains1_0,
/*9465*/          OPC_EmitConvertToTarget, 2,
/*9467*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::TCRETURNdi), 0|OPFL_Chain|OPFL_FlagInput|OPFL_Variadic2,
                      0/*#VTs*/, 2/*#Ops*/, 1, 3, 
                  // Src: (PPCtc_return (tglobaladdr:i32):$dst, (imm:i32):$imm) - Complexity = 9
                  // Dst: (TCRETURNdi (tglobaladdr:i32):$dst, (imm:i32):$imm)
                /*SwitchType*/ 19,  MVT::i64,// ->9496
/*9477*/          OPC_MoveParent,
/*9478*/          OPC_RecordChild2, // #2 = $imm
/*9479*/          OPC_MoveChild, 2,
/*9481*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*9484*/          OPC_MoveParent,
/*9485*/          OPC_EmitMergeInputChains1_0,
/*9486*/          OPC_EmitConvertToTarget, 2,
/*9488*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::TCRETURNdi8), 0|OPFL_Chain|OPFL_FlagInput|OPFL_Variadic2,
                      0/*#VTs*/, 2/*#Ops*/, 1, 3, 
                  // Src: (PPCtc_return (tglobaladdr:i64):$dst, (imm:i32):$imm) - Complexity = 9
                  // Dst: (TCRETURNdi8 (tglobaladdr:i64):$dst, (imm:i32):$imm)
                0, // EndSwitchType
              /*SwitchOpcode*/ 44,  TARGET_OPCODE(ISD::TargetExternalSymbol),// ->9544
/*9500*/        OPC_SwitchType /*2 cases */, 19,  MVT::i32,// ->9522
/*9503*/          OPC_MoveParent,
/*9504*/          OPC_RecordChild2, // #2 = $imm
/*9505*/          OPC_MoveChild, 2,
/*9507*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*9510*/          OPC_MoveParent,
/*9511*/          OPC_EmitMergeInputChains1_0,
/*9512*/          OPC_EmitConvertToTarget, 2,
/*9514*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::TCRETURNdi), 0|OPFL_Chain|OPFL_FlagInput|OPFL_Variadic2,
                      0/*#VTs*/, 2/*#Ops*/, 1, 3, 
                  // Src: (PPCtc_return (texternalsym:i32):$dst, (imm:i32):$imm) - Complexity = 9
                  // Dst: (TCRETURNdi (texternalsym:i32):$dst, (imm:i32):$imm)
                /*SwitchType*/ 19,  MVT::i64,// ->9543
/*9524*/          OPC_MoveParent,
/*9525*/          OPC_RecordChild2, // #2 = $imm
/*9526*/          OPC_MoveChild, 2,
/*9528*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*9531*/          OPC_MoveParent,
/*9532*/          OPC_EmitMergeInputChains1_0,
/*9533*/          OPC_EmitConvertToTarget, 2,
/*9535*/          OPC_MorphNodeTo, TARGET_OPCODE(PPC::TCRETURNdi8), 0|OPFL_Chain|OPFL_FlagInput|OPFL_Variadic2,
                      0/*#VTs*/, 2/*#Ops*/, 1, 3, 
                  // Src: (PPCtc_return (texternalsym:i64):$dst, (imm:i32):$imm) - Complexity = 9
                  // Dst: (TCRETURNdi8 (texternalsym:i64):$dst, (imm:i32):$imm)
                0, // EndSwitchType
              0, // EndSwitchOpcode
/*9545*/    /*Scope*/ 20, /*->9566*/
/*9546*/      OPC_CheckChild1Type, MVT::i32,
/*9548*/      OPC_RecordChild2, // #2 = $imm
/*9549*/      OPC_MoveChild, 2,
/*9551*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*9554*/      OPC_MoveParent,
/*9555*/      OPC_EmitMergeInputChains1_0,
/*9556*/      OPC_EmitConvertToTarget, 2,
/*9558*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::TCRETURNri), 0|OPFL_Chain|OPFL_FlagInput|OPFL_Variadic2,
                  0/*#VTs*/, 2/*#Ops*/, 1, 3, 
              // Src: (PPCtc_return CTRRC:i32:$dst, (imm:i32):$imm) - Complexity = 6
              // Dst: (TCRETURNri CTRRC:i32:$dst, (imm:i32):$imm)
/*9566*/    /*Scope*/ 20, /*->9587*/
/*9567*/      OPC_CheckChild1Type, MVT::i64,
/*9569*/      OPC_RecordChild2, // #2 = $imm
/*9570*/      OPC_MoveChild, 2,
/*9572*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*9575*/      OPC_MoveParent,
/*9576*/      OPC_EmitMergeInputChains1_0,
/*9577*/      OPC_EmitConvertToTarget, 2,
/*9579*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::TCRETURNri8), 0|OPFL_Chain|OPFL_FlagInput|OPFL_Variadic2,
                  0/*#VTs*/, 2/*#Ops*/, 1, 3, 
              // Src: (PPCtc_return CTRRC8:i64:$dst, (imm:i32):$imm) - Complexity = 6
              // Dst: (TCRETURNri8 CTRRC8:i64:$dst, (imm:i32):$imm)
/*9587*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 84|128,1/*212*/,  TARGET_OPCODE(PPCISD::VCMP),// ->9804
/*9592*/    OPC_RecordChild0, // #0 = $vA
/*9593*/    OPC_RecordChild1, // #1 = $vB
/*9594*/    OPC_MoveChild, 2,
/*9596*/    OPC_Scope, 14, /*->9612*/ // 13 children in Scope
/*9598*/      OPC_CheckInteger, 6, 
/*9600*/      OPC_MoveParent,
/*9601*/      OPC_CheckType, MVT::v16i8,
/*9603*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPEQUB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB, 6:i32) - Complexity = 8
              // Dst: (VCMPEQUB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*9612*/    /*Scope*/ 15, /*->9628*/
/*9613*/      OPC_CheckInteger, 6|128,6/*774*/, 
/*9616*/      OPC_MoveParent,
/*9617*/      OPC_CheckType, MVT::v16i8,
/*9619*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTSB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB, 774:i32) - Complexity = 8
              // Dst: (VCMPGTSB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*9628*/    /*Scope*/ 15, /*->9644*/
/*9629*/      OPC_CheckInteger, 6|128,4/*518*/, 
/*9632*/      OPC_MoveParent,
/*9633*/      OPC_CheckType, MVT::v16i8,
/*9635*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTUB), 0,
                  1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB, 518:i32) - Complexity = 8
              // Dst: (VCMPGTUB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*9644*/    /*Scope*/ 14, /*->9659*/
/*9645*/      OPC_CheckInteger, 70, 
/*9647*/      OPC_MoveParent,
/*9648*/      OPC_CheckType, MVT::v8i16,
/*9650*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPEQUH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB, 70:i32) - Complexity = 8
              // Dst: (VCMPEQUH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*9659*/    /*Scope*/ 15, /*->9675*/
/*9660*/      OPC_CheckInteger, 70|128,6/*838*/, 
/*9663*/      OPC_MoveParent,
/*9664*/      OPC_CheckType, MVT::v8i16,
/*9666*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTSH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB, 838:i32) - Complexity = 8
              // Dst: (VCMPGTSH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*9675*/    /*Scope*/ 15, /*->9691*/
/*9676*/      OPC_CheckInteger, 70|128,4/*582*/, 
/*9679*/      OPC_MoveParent,
/*9680*/      OPC_CheckType, MVT::v8i16,
/*9682*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTUH), 0,
                  1/*#VTs*/, MVT::v8i16, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB, 582:i32) - Complexity = 8
              // Dst: (VCMPGTUH:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*9691*/    /*Scope*/ 15, /*->9707*/
/*9692*/      OPC_CheckInteger, 6|128,1/*134*/, 
/*9695*/      OPC_MoveParent,
/*9696*/      OPC_CheckType, MVT::v4i32,
/*9698*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPEQUW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB, 134:i32) - Complexity = 8
              // Dst: (VCMPEQUW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*9707*/    /*Scope*/ 15, /*->9723*/
/*9708*/      OPC_CheckInteger, 6|128,7/*902*/, 
/*9711*/      OPC_MoveParent,
/*9712*/      OPC_CheckType, MVT::v4i32,
/*9714*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTSW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB, 902:i32) - Complexity = 8
              // Dst: (VCMPGTSW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*9723*/    /*Scope*/ 15, /*->9739*/
/*9724*/      OPC_CheckInteger, 6|128,5/*646*/, 
/*9727*/      OPC_MoveParent,
/*9728*/      OPC_CheckType, MVT::v4i32,
/*9730*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTUW), 0,
                  1/*#VTs*/, MVT::v4i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB, 646:i32) - Complexity = 8
              // Dst: (VCMPGTUW:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*9739*/    /*Scope*/ 15, /*->9755*/
/*9740*/      OPC_CheckInteger, 70|128,7/*966*/, 
/*9743*/      OPC_MoveParent,
/*9744*/      OPC_CheckType, MVT::v4f32,
/*9746*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPBFP), 0,
                  1/*#VTs*/, MVT::v4f32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB, 966:i32) - Complexity = 8
              // Dst: (VCMPBFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*9755*/    /*Scope*/ 15, /*->9771*/
/*9756*/      OPC_CheckInteger, 70|128,1/*198*/, 
/*9759*/      OPC_MoveParent,
/*9760*/      OPC_CheckType, MVT::v4f32,
/*9762*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPEQFP), 0,
                  1/*#VTs*/, MVT::v4f32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB, 198:i32) - Complexity = 8
              // Dst: (VCMPEQFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*9771*/    /*Scope*/ 15, /*->9787*/
/*9772*/      OPC_CheckInteger, 70|128,3/*454*/, 
/*9775*/      OPC_MoveParent,
/*9776*/      OPC_CheckType, MVT::v4f32,
/*9778*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGEFP), 0,
                  1/*#VTs*/, MVT::v4f32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB, 454:i32) - Complexity = 8
              // Dst: (VCMPGEFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*9787*/    /*Scope*/ 15, /*->9803*/
/*9788*/      OPC_CheckInteger, 70|128,5/*710*/, 
/*9791*/      OPC_MoveParent,
/*9792*/      OPC_CheckType, MVT::v4f32,
/*9794*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTFP), 0,
                  1/*#VTs*/, MVT::v4f32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB, 710:i32) - Complexity = 8
              // Dst: (VCMPGTFP:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*9803*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 97|128,1/*225*/,  TARGET_OPCODE(PPCISD::VCMPo),// ->10033
/*9808*/    OPC_RecordChild0, // #0 = $vA
/*9809*/    OPC_RecordChild1, // #1 = $vB
/*9810*/    OPC_MoveChild, 2,
/*9812*/    OPC_Scope, 15, /*->9829*/ // 13 children in Scope
/*9814*/      OPC_CheckInteger, 6, 
/*9816*/      OPC_MoveParent,
/*9817*/      OPC_CheckType, MVT::v16i8,
/*9819*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPEQUBo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v16i8, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB, 6:i32) - Complexity = 8
              // Dst: (VCMPEQUBo:v16i8:i32 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*9829*/    /*Scope*/ 16, /*->9846*/
/*9830*/      OPC_CheckInteger, 6|128,6/*774*/, 
/*9833*/      OPC_MoveParent,
/*9834*/      OPC_CheckType, MVT::v16i8,
/*9836*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTSBo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v16i8, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB, 774:i32) - Complexity = 8
              // Dst: (VCMPGTSBo:v16i8:i32 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*9846*/    /*Scope*/ 16, /*->9863*/
/*9847*/      OPC_CheckInteger, 6|128,4/*518*/, 
/*9850*/      OPC_MoveParent,
/*9851*/      OPC_CheckType, MVT::v16i8,
/*9853*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTUBo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v16i8, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB, 518:i32) - Complexity = 8
              // Dst: (VCMPGTUBo:v16i8:i32 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*9863*/    /*Scope*/ 15, /*->9879*/
/*9864*/      OPC_CheckInteger, 70, 
/*9866*/      OPC_MoveParent,
/*9867*/      OPC_CheckType, MVT::v8i16,
/*9869*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPEQUHo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v8i16, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB, 70:i32) - Complexity = 8
              // Dst: (VCMPEQUHo:v8i16:i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*9879*/    /*Scope*/ 16, /*->9896*/
/*9880*/      OPC_CheckInteger, 70|128,6/*838*/, 
/*9883*/      OPC_MoveParent,
/*9884*/      OPC_CheckType, MVT::v8i16,
/*9886*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTSHo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v8i16, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB, 838:i32) - Complexity = 8
              // Dst: (VCMPGTSHo:v8i16:i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*9896*/    /*Scope*/ 16, /*->9913*/
/*9897*/      OPC_CheckInteger, 70|128,4/*582*/, 
/*9900*/      OPC_MoveParent,
/*9901*/      OPC_CheckType, MVT::v8i16,
/*9903*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTUHo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v8i16, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v8i16 VRRC:v8i16:$vA, VRRC:v8i16:$vB, 582:i32) - Complexity = 8
              // Dst: (VCMPGTUHo:v8i16:i32 VRRC:v8i16:$vA, VRRC:v8i16:$vB)
/*9913*/    /*Scope*/ 16, /*->9930*/
/*9914*/      OPC_CheckInteger, 6|128,1/*134*/, 
/*9917*/      OPC_MoveParent,
/*9918*/      OPC_CheckType, MVT::v4i32,
/*9920*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPEQUWo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v4i32, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB, 134:i32) - Complexity = 8
              // Dst: (VCMPEQUWo:v4i32:i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*9930*/    /*Scope*/ 16, /*->9947*/
/*9931*/      OPC_CheckInteger, 6|128,7/*902*/, 
/*9934*/      OPC_MoveParent,
/*9935*/      OPC_CheckType, MVT::v4i32,
/*9937*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTSWo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v4i32, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB, 902:i32) - Complexity = 8
              // Dst: (VCMPGTSWo:v4i32:i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*9947*/    /*Scope*/ 16, /*->9964*/
/*9948*/      OPC_CheckInteger, 6|128,5/*646*/, 
/*9951*/      OPC_MoveParent,
/*9952*/      OPC_CheckType, MVT::v4i32,
/*9954*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTUWo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v4i32, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v4i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB, 646:i32) - Complexity = 8
              // Dst: (VCMPGTUWo:v4i32:i32 VRRC:v4i32:$vA, VRRC:v4i32:$vB)
/*9964*/    /*Scope*/ 16, /*->9981*/
/*9965*/      OPC_CheckInteger, 70|128,7/*966*/, 
/*9968*/      OPC_MoveParent,
/*9969*/      OPC_CheckType, MVT::v4f32,
/*9971*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPBFPo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v4f32, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB, 966:i32) - Complexity = 8
              // Dst: (VCMPBFPo:v4f32:i32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*9981*/    /*Scope*/ 16, /*->9998*/
/*9982*/      OPC_CheckInteger, 70|128,1/*198*/, 
/*9985*/      OPC_MoveParent,
/*9986*/      OPC_CheckType, MVT::v4f32,
/*9988*/      OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPEQFPo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v4f32, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB, 198:i32) - Complexity = 8
              // Dst: (VCMPEQFPo:v4f32:i32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*9998*/    /*Scope*/ 16, /*->10015*/
/*9999*/      OPC_CheckInteger, 70|128,3/*454*/, 
/*10002*/     OPC_MoveParent,
/*10003*/     OPC_CheckType, MVT::v4f32,
/*10005*/     OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGEFPo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v4f32, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB, 454:i32) - Complexity = 8
              // Dst: (VCMPGEFPo:v4f32:i32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*10015*/   /*Scope*/ 16, /*->10032*/
/*10016*/     OPC_CheckInteger, 70|128,5/*710*/, 
/*10019*/     OPC_MoveParent,
/*10020*/     OPC_CheckType, MVT::v4f32,
/*10022*/     OPC_MorphNodeTo, TARGET_OPCODE(PPC::VCMPGTFPo), 0|OPFL_FlagOutput,
                  2/*#VTs*/, MVT::v4f32, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (PPCvcmp_o:v4f32 VRRC:v4f32:$vA, VRRC:v4f32:$vB, 710:i32) - Complexity = 8
              // Dst: (VCMPGTFPo:v4f32:i32 VRRC:v4f32:$vA, VRRC:v4f32:$vB)
/*10032*/   0, /*End of Scope*/
          /*SwitchOpcode*/ 52|128,2/*308*/,  TARGET_OPCODE(ISD::VECTOR_SHUFFLE),// ->10345
/*10037*/   OPC_Scope, 73, /*->10112*/ // 4 children in Scope
/*10039*/     OPC_RecordNode, // #0 = $UIMM
/*10040*/     OPC_RecordChild0, // #1 = $vB
/*10041*/     OPC_MoveChild, 1,
/*10043*/     OPC_CheckOpcode, TARGET_OPCODE(ISD::UNDEF),
/*10046*/     OPC_MoveParent,
/*10047*/     OPC_CheckType, MVT::v16i8,
/*10049*/     OPC_Scope, 14, /*->10065*/ // 4 children in Scope
/*10051*/       OPC_CheckPredicate, 67, // Predicate_vspltb_shuffle
/*10053*/       OPC_EmitNodeXForm, 9, 0, // VSPLTB_get_imm
/*10056*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSPLTB), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 2, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vB, (undef:v16i8))<<P:Predicate_vspltb_shuffle>><<X:VSPLTB_get_imm>>:$UIMM - Complexity = 7
                // Dst: (VSPLTB:v16i8 (VSPLTB_get_imm:i32 (vector_shuffle:v16i8 VRRC:v16i8:$vB, (undef:v16i8)):$UIMM), VRRC:v16i8:$vB)
/*10065*/     /*Scope*/ 14, /*->10080*/
/*10066*/       OPC_CheckPredicate, 68, // Predicate_vsplth_shuffle
/*10068*/       OPC_EmitNodeXForm, 10, 0, // VSPLTH_get_imm
/*10071*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSPLTH), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 2, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vB, (undef:v16i8))<<P:Predicate_vsplth_shuffle>><<X:VSPLTH_get_imm>>:$UIMM - Complexity = 7
                // Dst: (VSPLTH:v16i8 (VSPLTH_get_imm:i32 (vector_shuffle:v16i8 VRRC:v16i8:$vB, (undef:v16i8)):$UIMM), VRRC:v16i8:$vB)
/*10080*/     /*Scope*/ 14, /*->10095*/
/*10081*/       OPC_CheckPredicate, 69, // Predicate_vspltw_shuffle
/*10083*/       OPC_EmitNodeXForm, 11, 0, // VSPLTW_get_imm
/*10086*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSPLTW), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 2, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vB, (undef:v16i8))<<P:Predicate_vspltw_shuffle>><<X:VSPLTW_get_imm>>:$UIMM - Complexity = 7
                // Dst: (VSPLTW:v16i8 (VSPLTW_get_imm:i32 (vector_shuffle:v16i8 VRRC:v16i8:$vB, (undef:v16i8)):$UIMM), VRRC:v16i8:$vB)
/*10095*/     /*Scope*/ 15, /*->10111*/
/*10096*/       OPC_CheckPredicate, 70, // Predicate_vsldoi_unary_shuffle
/*10098*/       OPC_EmitNodeXForm, 12, 0, // VSLDOI_unary_get_imm
/*10101*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSLDOI), 0,
                    1/*#VTs*/, MVT::v16i8, 3/*#Ops*/, 1, 1, 2, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, (undef:v16i8))<<P:Predicate_vsldoi_unary_shuffle>><<X:VSLDOI_unary_get_imm>>:$in - Complexity = 7
                // Dst: (VSLDOI:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vA, (VSLDOI_unary_get_imm:i32 VRRC:v16i8:$in))
/*10111*/     0, /*End of Scope*/
/*10112*/   /*Scope*/ 107, /*->10220*/
/*10113*/     OPC_RecordChild0, // #0 = $vA
/*10114*/     OPC_MoveChild, 1,
/*10116*/     OPC_CheckOpcode, TARGET_OPCODE(ISD::UNDEF),
/*10119*/     OPC_MoveParent,
/*10120*/     OPC_CheckType, MVT::v16i8,
/*10122*/     OPC_Scope, 11, /*->10135*/ // 8 children in Scope
/*10124*/       OPC_CheckPredicate, 71, // Predicate_vpkuwum_unary_shuffle
/*10126*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKUWUM), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 0, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, (undef:v16i8))<<P:Predicate_vpkuwum_unary_shuffle>> - Complexity = 7
                // Dst: (VPKUWUM:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vA)
/*10135*/     /*Scope*/ 11, /*->10147*/
/*10136*/       OPC_CheckPredicate, 72, // Predicate_vpkuhum_unary_shuffle
/*10138*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKUHUM), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 0, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, (undef:v16i8))<<P:Predicate_vpkuhum_unary_shuffle>> - Complexity = 7
                // Dst: (VPKUHUM:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vA)
/*10147*/     /*Scope*/ 11, /*->10159*/
/*10148*/       OPC_CheckPredicate, 73, // Predicate_vmrglb_unary_shuffle
/*10150*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGLB), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 0, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, (undef:v16i8))<<P:Predicate_vmrglb_unary_shuffle>> - Complexity = 7
                // Dst: (VMRGLB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vA)
/*10159*/     /*Scope*/ 11, /*->10171*/
/*10160*/       OPC_CheckPredicate, 74, // Predicate_vmrglh_unary_shuffle
/*10162*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGLH), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 0, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, (undef:v16i8))<<P:Predicate_vmrglh_unary_shuffle>> - Complexity = 7
                // Dst: (VMRGLH:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vA)
/*10171*/     /*Scope*/ 11, /*->10183*/
/*10172*/       OPC_CheckPredicate, 75, // Predicate_vmrglw_unary_shuffle
/*10174*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGLW), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 0, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, (undef:v16i8))<<P:Predicate_vmrglw_unary_shuffle>> - Complexity = 7
                // Dst: (VMRGLW:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vA)
/*10183*/     /*Scope*/ 11, /*->10195*/
/*10184*/       OPC_CheckPredicate, 76, // Predicate_vmrghb_unary_shuffle
/*10186*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGHB), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 0, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, (undef:v16i8))<<P:Predicate_vmrghb_unary_shuffle>> - Complexity = 7
                // Dst: (VMRGHB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vA)
/*10195*/     /*Scope*/ 11, /*->10207*/
/*10196*/       OPC_CheckPredicate, 77, // Predicate_vmrghh_unary_shuffle
/*10198*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGHH), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 0, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, (undef:v16i8))<<P:Predicate_vmrghh_unary_shuffle>> - Complexity = 7
                // Dst: (VMRGHH:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vA)
/*10207*/     /*Scope*/ 11, /*->10219*/
/*10208*/       OPC_CheckPredicate, 78, // Predicate_vmrghw_unary_shuffle
/*10210*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGHW), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 0, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, (undef:v16i8))<<P:Predicate_vmrghw_unary_shuffle>> - Complexity = 7
                // Dst: (VMRGHW:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vA)
/*10219*/     0, /*End of Scope*/
/*10220*/   /*Scope*/ 20, /*->10241*/
/*10221*/     OPC_RecordNode, // #0 = $SH
/*10222*/     OPC_RecordChild0, // #1 = $vA
/*10223*/     OPC_RecordChild1, // #2 = $vB
/*10224*/     OPC_CheckPredicate, 79, // Predicate_vsldoi_shuffle
/*10226*/     OPC_CheckType, MVT::v16i8,
/*10228*/     OPC_EmitNodeXForm, 13, 0, // VSLDOI_get_imm
/*10231*/     OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSLDOI), 0,
                  1/*#VTs*/, MVT::v16i8, 3/*#Ops*/, 1, 2, 3, 
              // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)<<P:Predicate_vsldoi_shuffle>><<X:VSLDOI_get_imm>>:$SH - Complexity = 4
              // Dst: (VSLDOI:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB, (VSLDOI_get_imm:i32 (vector_shuffle:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB):$SH))
/*10241*/   /*Scope*/ 102, /*->10344*/
/*10242*/     OPC_RecordChild0, // #0 = $vA
/*10243*/     OPC_RecordChild1, // #1 = $vB
/*10244*/     OPC_CheckType, MVT::v16i8,
/*10246*/     OPC_Scope, 11, /*->10259*/ // 8 children in Scope
/*10248*/       OPC_CheckPredicate, 80, // Predicate_vmrghb_shuffle
/*10250*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGHB), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)<<P:Predicate_vmrghb_shuffle>> - Complexity = 4
                // Dst: (VMRGHB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*10259*/     /*Scope*/ 11, /*->10271*/
/*10260*/       OPC_CheckPredicate, 81, // Predicate_vmrghh_shuffle
/*10262*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGHH), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)<<P:Predicate_vmrghh_shuffle>> - Complexity = 4
                // Dst: (VMRGHH:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*10271*/     /*Scope*/ 11, /*->10283*/
/*10272*/       OPC_CheckPredicate, 82, // Predicate_vmrghw_shuffle
/*10274*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGHW), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)<<P:Predicate_vmrghw_shuffle>> - Complexity = 4
                // Dst: (VMRGHW:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*10283*/     /*Scope*/ 11, /*->10295*/
/*10284*/       OPC_CheckPredicate, 83, // Predicate_vmrglb_shuffle
/*10286*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGLB), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)<<P:Predicate_vmrglb_shuffle>> - Complexity = 4
                // Dst: (VMRGLB:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*10295*/     /*Scope*/ 11, /*->10307*/
/*10296*/       OPC_CheckPredicate, 84, // Predicate_vmrglh_shuffle
/*10298*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGLH), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)<<P:Predicate_vmrglh_shuffle>> - Complexity = 4
                // Dst: (VMRGLH:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*10307*/     /*Scope*/ 11, /*->10319*/
/*10308*/       OPC_CheckPredicate, 85, // Predicate_vmrglw_shuffle
/*10310*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMRGLW), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)<<P:Predicate_vmrglw_shuffle>> - Complexity = 4
                // Dst: (VMRGLW:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*10319*/     /*Scope*/ 11, /*->10331*/
/*10320*/       OPC_CheckPredicate, 86, // Predicate_vpkuhum_shuffle
/*10322*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKUHUM), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)<<P:Predicate_vpkuhum_shuffle>> - Complexity = 4
                // Dst: (VPKUHUM:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*10331*/     /*Scope*/ 11, /*->10343*/
/*10332*/       OPC_CheckPredicate, 87, // Predicate_vpkuwum_shuffle
/*10334*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPKUWUM), 0,
                    1/*#VTs*/, MVT::v16i8, 2/*#Ops*/, 0, 1, 
                // Src: (vector_shuffle:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)<<P:Predicate_vpkuwum_shuffle>> - Complexity = 4
                // Dst: (VPKUWUM:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB)
/*10343*/     0, /*End of Scope*/
/*10344*/   0, /*End of Scope*/
          /*SwitchOpcode*/ 17,  TARGET_OPCODE(ISD::CALLSEQ_START),// ->10365
/*10348*/   OPC_RecordNode,   // #0 = 'callseq_start' chained node
/*10349*/   OPC_RecordChild1, // #1 = $amt
/*10350*/   OPC_MoveChild, 1,
/*10352*/   OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*10355*/   OPC_MoveParent,
/*10356*/   OPC_EmitMergeInputChains1_0,
/*10357*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::ADJCALLSTACKDOWN), 0|OPFL_Chain|OPFL_FlagOutput,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
            // Src: (callseq_start (timm:i32):$amt) - Complexity = 6
            // Dst: (ADJCALLSTACKDOWN:i32 (timm:i32):$amt)
          /*SwitchOpcode*/ 98,  TARGET_OPCODE(PPCISD::CALL_Darwin),// ->10466
/*10368*/   OPC_RecordNode,   // #0 = 'PPCcall_Darwin' chained node
/*10369*/   OPC_CaptureFlagInput,
/*10370*/   OPC_RecordChild1, // #1 = $func
/*10371*/   OPC_MoveChild, 1,
/*10373*/   OPC_SwitchOpcode /*3 cases */, 30,  TARGET_OPCODE(ISD::Constant),// ->10407
/*10377*/     OPC_SwitchType /*2 cases */, 12,  MVT::i32,// ->10392
/*10380*/       OPC_MoveParent,
/*10381*/       OPC_EmitMergeInputChains1_0,
/*10382*/       OPC_EmitConvertToTarget, 1,
/*10384*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BLA_Darwin), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2, 
                // Src: (PPCcall_Darwin (imm:i32):$func) - Complexity = 6
                // Dst: (BLA_Darwin:i32 (imm:i32):$func)
              /*SwitchType*/ 12,  MVT::i64,// ->10406
/*10394*/       OPC_MoveParent,
/*10395*/       OPC_EmitMergeInputChains1_0,
/*10396*/       OPC_EmitConvertToTarget, 1,
/*10398*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BLA8_Darwin), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 2, 
                // Src: (PPCcall_Darwin (imm:i64):$func) - Complexity = 6
                // Dst: (BLA8_Darwin:i64 (imm:i64):$func)
              0, // EndSwitchType
            /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->10436
/*10410*/     OPC_SwitchType /*2 cases */, 10,  MVT::i32,// ->10423
/*10413*/       OPC_MoveParent,
/*10414*/       OPC_EmitMergeInputChains1_0,
/*10415*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BL_Darwin), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
                // Src: (PPCcall_Darwin (tglobaladdr:i32):$dst) - Complexity = 6
                // Dst: (BL_Darwin:i32 (tglobaladdr:i32):$dst)
              /*SwitchType*/ 10,  MVT::i64,// ->10435
/*10425*/       OPC_MoveParent,
/*10426*/       OPC_EmitMergeInputChains1_0,
/*10427*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BL8_Darwin), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 1, 
                // Src: (PPCcall_Darwin (tglobaladdr:i64):$dst) - Complexity = 6
                // Dst: (BL8_Darwin:i64 (tglobaladdr:i64):$dst)
              0, // EndSwitchType
            /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::TargetExternalSymbol),// ->10465
/*10439*/     OPC_SwitchType /*2 cases */, 10,  MVT::i32,// ->10452
/*10442*/       OPC_MoveParent,
/*10443*/       OPC_EmitMergeInputChains1_0,
/*10444*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BL_Darwin), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
                // Src: (PPCcall_Darwin (texternalsym:i32):$dst) - Complexity = 6
                // Dst: (BL_Darwin:i32 (texternalsym:i32):$dst)
              /*SwitchType*/ 10,  MVT::i64,// ->10464
/*10454*/       OPC_MoveParent,
/*10455*/       OPC_EmitMergeInputChains1_0,
/*10456*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BL8_Darwin), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 1, 
                // Src: (PPCcall_Darwin (texternalsym:i64):$dst) - Complexity = 6
                // Dst: (BL8_Darwin:i64 (texternalsym:i64):$dst)
              0, // EndSwitchType
            0, // EndSwitchOpcode
          /*SwitchOpcode*/ 98,  TARGET_OPCODE(PPCISD::CALL_SVR4),// ->10567
/*10469*/   OPC_RecordNode,   // #0 = 'PPCcall_SVR4' chained node
/*10470*/   OPC_CaptureFlagInput,
/*10471*/   OPC_RecordChild1, // #1 = $func
/*10472*/   OPC_MoveChild, 1,
/*10474*/   OPC_SwitchOpcode /*3 cases */, 30,  TARGET_OPCODE(ISD::Constant),// ->10508
/*10478*/     OPC_SwitchType /*2 cases */, 12,  MVT::i32,// ->10493
/*10481*/       OPC_MoveParent,
/*10482*/       OPC_EmitMergeInputChains1_0,
/*10483*/       OPC_EmitConvertToTarget, 1,
/*10485*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BLA_SVR4), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2, 
                // Src: (PPCcall_SVR4 (imm:i32):$func) - Complexity = 6
                // Dst: (BLA_SVR4:i32 (imm:i32):$func)
              /*SwitchType*/ 12,  MVT::i64,// ->10507
/*10495*/       OPC_MoveParent,
/*10496*/       OPC_EmitMergeInputChains1_0,
/*10497*/       OPC_EmitConvertToTarget, 1,
/*10499*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BLA8_ELF), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 2, 
                // Src: (PPCcall_SVR4 (imm:i64):$func) - Complexity = 6
                // Dst: (BLA8_ELF:i64 (imm:i64):$func)
              0, // EndSwitchType
            /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->10537
/*10511*/     OPC_SwitchType /*2 cases */, 10,  MVT::i32,// ->10524
/*10514*/       OPC_MoveParent,
/*10515*/       OPC_EmitMergeInputChains1_0,
/*10516*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BL_SVR4), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
                // Src: (PPCcall_SVR4 (tglobaladdr:i32):$dst) - Complexity = 6
                // Dst: (BL_SVR4:i32 (tglobaladdr:i32):$dst)
              /*SwitchType*/ 10,  MVT::i64,// ->10536
/*10526*/       OPC_MoveParent,
/*10527*/       OPC_EmitMergeInputChains1_0,
/*10528*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BL8_ELF), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 1, 
                // Src: (PPCcall_SVR4 (tglobaladdr:i64):$dst) - Complexity = 6
                // Dst: (BL8_ELF:i64 (tglobaladdr:i64):$dst)
              0, // EndSwitchType
            /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::TargetExternalSymbol),// ->10566
/*10540*/     OPC_SwitchType /*2 cases */, 10,  MVT::i32,// ->10553
/*10543*/       OPC_MoveParent,
/*10544*/       OPC_EmitMergeInputChains1_0,
/*10545*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BL_SVR4), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
                // Src: (PPCcall_SVR4 (texternalsym:i32):$dst) - Complexity = 6
                // Dst: (BL_SVR4:i32 (texternalsym:i32):$dst)
              /*SwitchType*/ 10,  MVT::i64,// ->10565
/*10555*/       OPC_MoveParent,
/*10556*/       OPC_EmitMergeInputChains1_0,
/*10557*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::BL8_ELF), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 1, 
                // Src: (PPCcall_SVR4 (texternalsym:i64):$dst) - Complexity = 6
                // Dst: (BL8_ELF:i64 (texternalsym:i64):$dst)
              0, // EndSwitchType
            0, // EndSwitchOpcode
          /*SwitchOpcode*/ 19,  TARGET_OPCODE(PPCISD::MTFSB0),// ->10589
/*10570*/   OPC_CaptureFlagInput,
/*10571*/   OPC_RecordChild0, // #0 = $FM
/*10572*/   OPC_MoveChild, 0,
/*10574*/   OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*10577*/   OPC_CheckType, MVT::i32,
/*10579*/   OPC_MoveParent,
/*10580*/   OPC_EmitConvertToTarget, 0,
/*10582*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::MTFSB0), 0|OPFL_FlagInput|OPFL_FlagOutput,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (PPCmtfsb0 (imm:i32):$FM) - Complexity = 6
            // Dst: (MTFSB0 (imm:i32):$FM)
          /*SwitchOpcode*/ 19,  TARGET_OPCODE(PPCISD::MTFSB1),// ->10611
/*10592*/   OPC_CaptureFlagInput,
/*10593*/   OPC_RecordChild0, // #0 = $FM
/*10594*/   OPC_MoveChild, 0,
/*10596*/   OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*10599*/   OPC_CheckType, MVT::i32,
/*10601*/   OPC_MoveParent,
/*10602*/   OPC_EmitConvertToTarget, 0,
/*10604*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::MTFSB1), 0|OPFL_FlagInput|OPFL_FlagOutput,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (PPCmtfsb1 (imm:i32):$FM) - Complexity = 6
            // Dst: (MTFSB1 (imm:i32):$FM)
          /*SwitchOpcode*/ 63,  TARGET_OPCODE(ISD::BUILD_VECTOR),// ->10677
/*10614*/   OPC_Scope, 48, /*->10664*/ // 2 children in Scope
/*10616*/     OPC_RecordNode, // #0 = $SIMM
/*10617*/     OPC_SwitchType /*3 cases */, 13,  MVT::v16i8,// ->10633
/*10620*/       OPC_CheckPredicate, 88, // Predicate_vecspltisb
/*10622*/       OPC_EmitNodeXForm, 14, 0, // VSPLTISB_get_imm
/*10625*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSPLTISB), 0,
                    1/*#VTs*/, MVT::v16i8, 1/*#Ops*/, 1, 
                // Src: (build_vector:v16i8)<<P:Predicate_vecspltisb>><<X:VSPLTISB_get_imm>>:$SIMM - Complexity = 4
                // Dst: (VSPLTISB:v16i8 (VSPLTISB_get_imm:i32 (build_vector:v16i8):$SIMM))
              /*SwitchType*/ 13,  MVT::v8i16,// ->10648
/*10635*/       OPC_CheckPredicate, 89, // Predicate_vecspltish
/*10637*/       OPC_EmitNodeXForm, 15, 0, // VSPLTISH_get_imm
/*10640*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSPLTISH), 0,
                    1/*#VTs*/, MVT::v8i16, 1/*#Ops*/, 1, 
                // Src: (build_vector:v8i16)<<P:Predicate_vecspltish>><<X:VSPLTISH_get_imm>>:$SIMM - Complexity = 4
                // Dst: (VSPLTISH:v8i16 (VSPLTISH_get_imm:i32 (build_vector:v8i16):$SIMM))
              /*SwitchType*/ 13,  MVT::v4i32,// ->10663
/*10650*/       OPC_CheckPredicate, 90, // Predicate_vecspltisw
/*10652*/       OPC_EmitNodeXForm, 16, 0, // VSPLTISW_get_imm
/*10655*/       OPC_MorphNodeTo, TARGET_OPCODE(PPC::VSPLTISW), 0,
                    1/*#VTs*/, MVT::v4i32, 1/*#Ops*/, 1, 
                // Src: (build_vector:v4i32)<<P:Predicate_vecspltisw>><<X:VSPLTISW_get_imm>>:$SIMM - Complexity = 4
                // Dst: (VSPLTISW:v4i32 (VSPLTISW_get_imm:i32 (build_vector:v4i32):$SIMM))
              0, // EndSwitchType
/*10664*/   /*Scope*/ 11, /*->10676*/
/*10665*/     OPC_CheckPredicate, 91, // Predicate_immAllZerosV
/*10667*/     OPC_CheckType, MVT::v4i32,
/*10669*/     OPC_MorphNodeTo, TARGET_OPCODE(PPC::V_SET0), 0,
                  1/*#VTs*/, MVT::v4i32, 0/*#Ops*/, 
              // Src: (build_vector:v4i32)<<P:Predicate_immAllZerosV>> - Complexity = 4
              // Dst: (V_SET0:v4i32)
/*10676*/   0, /*End of Scope*/
          /*SwitchOpcode*/ 83,  TARGET_OPCODE(ISD::BIT_CONVERT),// ->10763
/*10680*/   OPC_RecordChild0, // #0 = $src
/*10681*/   OPC_Scope, 19, /*->10702*/ // 4 children in Scope
/*10683*/     OPC_CheckChild0Type, MVT::v8i16,
/*10685*/     OPC_SwitchType /*3 cases */, 3,  MVT::v16i8,// ->10691
/*10688*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v16i8 VRRC:v8i16:$src) - Complexity = 3
                // Dst: VRRC:v16i8:$src
              /*SwitchType*/ 3,  MVT::v4i32,// ->10696
/*10693*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v4i32 VRRC:v8i16:$src) - Complexity = 3
                // Dst: VRRC:v4i32:$src
              /*SwitchType*/ 3,  MVT::v4f32,// ->10701
/*10698*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v4f32 VRRC:v8i16:$src) - Complexity = 3
                // Dst: VRRC:v4f32:$src
              0, // EndSwitchType
/*10702*/   /*Scope*/ 19, /*->10722*/
/*10703*/     OPC_CheckChild0Type, MVT::v4i32,
/*10705*/     OPC_SwitchType /*3 cases */, 3,  MVT::v16i8,// ->10711
/*10708*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v16i8 VRRC:v4i32:$src) - Complexity = 3
                // Dst: VRRC:v16i8:$src
              /*SwitchType*/ 3,  MVT::v8i16,// ->10716
/*10713*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v8i16 VRRC:v4i32:$src) - Complexity = 3
                // Dst: VRRC:v8i16:$src
              /*SwitchType*/ 3,  MVT::v4f32,// ->10721
/*10718*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v4f32 VRRC:v4i32:$src) - Complexity = 3
                // Dst: VRRC:v4f32:$src
              0, // EndSwitchType
/*10722*/   /*Scope*/ 19, /*->10742*/
/*10723*/     OPC_CheckChild0Type, MVT::v4f32,
/*10725*/     OPC_SwitchType /*3 cases */, 3,  MVT::v16i8,// ->10731
/*10728*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v16i8 VRRC:v4f32:$src) - Complexity = 3
                // Dst: VRRC:v16i8:$src
              /*SwitchType*/ 3,  MVT::v8i16,// ->10736
/*10733*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v8i16 VRRC:v4f32:$src) - Complexity = 3
                // Dst: VRRC:v8i16:$src
              /*SwitchType*/ 3,  MVT::v4i32,// ->10741
/*10738*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v4i32 VRRC:v4f32:$src) - Complexity = 3
                // Dst: VRRC:v4i32:$src
              0, // EndSwitchType
/*10742*/   /*Scope*/ 19, /*->10762*/
/*10743*/     OPC_CheckChild0Type, MVT::v16i8,
/*10745*/     OPC_SwitchType /*3 cases */, 3,  MVT::v8i16,// ->10751
/*10748*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v8i16 VRRC:v16i8:$src) - Complexity = 3
                // Dst: VRRC:v8i16:$src
              /*SwitchType*/ 3,  MVT::v4i32,// ->10756
/*10753*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v4i32 VRRC:v16i8:$src) - Complexity = 3
                // Dst: VRRC:v4i32:$src
              /*SwitchType*/ 3,  MVT::v4f32,// ->10761
/*10758*/       OPC_CompleteMatch, 1, 0, 
                // Src: (bitconvert:v4f32 VRRC:v16i8:$src) - Complexity = 3
                // Dst: VRRC:v4f32:$src
              0, // EndSwitchType
/*10762*/   0, /*End of Scope*/
          /*SwitchOpcode*/ 17,  TARGET_OPCODE(PPCISD::RET_FLAG),// ->10783
/*10766*/   OPC_RecordNode,   // #0 = 'retflag' chained node
/*10767*/   OPC_CaptureFlagInput,
/*10768*/   OPC_EmitMergeInputChains1_0,
/*10769*/   OPC_EmitInteger, MVT::i32, 20, 
/*10772*/   OPC_EmitRegister, MVT::i32, 0 /*zero_reg*/,
/*10775*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::BLR), 0|OPFL_Chain|OPFL_FlagInput|OPFL_Variadic0,
                0/*#VTs*/, 2/*#Ops*/, 1, 2, 
            // Src: (retflag) - Complexity = 3
            // Dst: (BLR)
          /*SwitchOpcode*/ 16,  TARGET_OPCODE(ISD::BR),// ->10802
/*10786*/   OPC_RecordNode,   // #0 = 'br' chained node
/*10787*/   OPC_RecordChild1, // #1 = $dst
/*10788*/   OPC_MoveChild, 1,
/*10790*/   OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*10793*/   OPC_MoveParent,
/*10794*/   OPC_EmitMergeInputChains1_0,
/*10795*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::B), 0|OPFL_Chain,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (br (bb:Other):$dst) - Complexity = 3
            // Dst: (B (bb:Other):$dst)
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(PPCISD::BCTRL_Darwin),// ->10831
/*10805*/   OPC_RecordNode,   // #0 = 'PPCbctrl_Darwin' chained node
/*10806*/   OPC_CaptureFlagInput,
/*10807*/   OPC_Scope, 10, /*->10819*/ // 2 children in Scope
/*10809*/     OPC_CheckPatternPredicate, 1, // (!PPCSubTarget.isPPC64())
/*10811*/     OPC_EmitMergeInputChains1_0,
/*10812*/     OPC_MorphNodeTo, TARGET_OPCODE(PPC::BCTRL_Darwin), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic0,
                  1/*#VTs*/, MVT::i32, 0/*#Ops*/, 
              // Src: (PPCbctrl_Darwin) - Complexity = 3
              // Dst: (BCTRL_Darwin:i32)
/*10819*/   /*Scope*/ 10, /*->10830*/
/*10820*/     OPC_CheckPatternPredicate, 2, // (PPCSubTarget.isPPC64())
/*10822*/     OPC_EmitMergeInputChains1_0,
/*10823*/     OPC_MorphNodeTo, TARGET_OPCODE(PPC::BCTRL8_Darwin), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic0,
                  1/*#VTs*/, MVT::i64, 0/*#Ops*/, 
              // Src: (PPCbctrl_Darwin) - Complexity = 3
              // Dst: (BCTRL8_Darwin:i64)
/*10830*/   0, /*End of Scope*/
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(PPCISD::BCTRL_SVR4),// ->10860
/*10834*/   OPC_RecordNode,   // #0 = 'PPCbctrl_SVR4' chained node
/*10835*/   OPC_CaptureFlagInput,
/*10836*/   OPC_Scope, 10, /*->10848*/ // 2 children in Scope
/*10838*/     OPC_CheckPatternPredicate, 1, // (!PPCSubTarget.isPPC64())
/*10840*/     OPC_EmitMergeInputChains1_0,
/*10841*/     OPC_MorphNodeTo, TARGET_OPCODE(PPC::BCTRL_SVR4), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic0,
                  1/*#VTs*/, MVT::i32, 0/*#Ops*/, 
              // Src: (PPCbctrl_SVR4) - Complexity = 3
              // Dst: (BCTRL_SVR4:i32)
/*10848*/   /*Scope*/ 10, /*->10859*/
/*10849*/     OPC_CheckPatternPredicate, 2, // (PPCSubTarget.isPPC64())
/*10851*/     OPC_EmitMergeInputChains1_0,
/*10852*/     OPC_MorphNodeTo, TARGET_OPCODE(PPC::BCTRL8_ELF), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic0,
                  1/*#VTs*/, MVT::i64, 0/*#Ops*/, 
              // Src: (PPCbctrl_SVR4) - Complexity = 3
              // Dst: (BCTRL8_ELF:i64)
/*10859*/   0, /*End of Scope*/
          /*SwitchOpcode*/ 8,  TARGET_OPCODE(ISD::TRAP),// ->10871
/*10863*/   OPC_RecordNode,   // #0 = 'trap' chained node
/*10864*/   OPC_EmitMergeInputChains1_0,
/*10865*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::TRAP), 0|OPFL_Chain,
                0/*#VTs*/, 0/*#Ops*/, 
            // Src: (trap) - Complexity = 3
            // Dst: (TRAP)
          /*SwitchOpcode*/ 29,  TARGET_OPCODE(PPCISD::MTCTR),// ->10903
/*10874*/   OPC_RecordNode,   // #0 = 'PPCmtctr' chained node
/*10875*/   OPC_CaptureFlagInput,
/*10876*/   OPC_RecordChild1, // #1 = $rS
/*10877*/   OPC_Scope, 11, /*->10890*/ // 2 children in Scope
/*10879*/     OPC_CheckChild1Type, MVT::i32,
/*10881*/     OPC_EmitMergeInputChains1_0,
/*10882*/     OPC_MorphNodeTo, TARGET_OPCODE(PPC::MTCTR), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
              // Src: (PPCmtctr GPRC:i32:$rS) - Complexity = 3
              // Dst: (MTCTR:i32 GPRC:i32:$rS)
/*10890*/   /*Scope*/ 11, /*->10902*/
/*10891*/     OPC_CheckChild1Type, MVT::i64,
/*10893*/     OPC_EmitMergeInputChains1_0,
/*10894*/     OPC_MorphNodeTo, TARGET_OPCODE(PPC::MTCTR8), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput,
                  1/*#VTs*/, MVT::i64, 1/*#Ops*/, 1, 
              // Src: (PPCmtctr G8RC:i64:$rS) - Complexity = 3
              // Dst: (MTCTR8:i64 G8RC:i64:$rS)
/*10902*/   0, /*End of Scope*/
          /*SwitchOpcode*/ 15,  TARGET_OPCODE(PPCISD::VPERM),// ->10921
/*10906*/   OPC_RecordChild0, // #0 = $vA
/*10907*/   OPC_RecordChild1, // #1 = $vB
/*10908*/   OPC_RecordChild2, // #2 = $vC
/*10909*/   OPC_CheckType, MVT::v16i8,
/*10911*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::VPERM), 0,
                1/*#VTs*/, MVT::v16i8, 3/*#Ops*/, 0, 1, 2, 
            // Src: (PPCvperm:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB, VRRC:v16i8:$vC) - Complexity = 3
            // Dst: (VPERM:v16i8 VRRC:v16i8:$vA, VRRC:v16i8:$vB, VRRC:v16i8:$vC)
          /*SwitchOpcode*/ 15,  TARGET_OPCODE(PPCISD::VMADDFP),// ->10939
/*10924*/   OPC_RecordChild0, // #0 = $A
/*10925*/   OPC_RecordChild1, // #1 = $B
/*10926*/   OPC_RecordChild2, // #2 = $C
/*10927*/   OPC_CheckType, MVT::v4f32,
/*10929*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::VMADDFP), 0,
                1/*#VTs*/, MVT::v4f32, 3/*#Ops*/, 0, 1, 2, 
            // Src: (PPCvmaddfp:v4f32 VRRC:v4f32:$A, VRRC:v4f32:$B, VRRC:v4f32:$C) - Complexity = 3
            // Dst: (VMADDFP:v4f32 VRRC:v4f32:$A, VRRC:v4f32:$B, VRRC:v4f32:$C)
          /*SwitchOpcode*/ 15,  TARGET_OPCODE(PPCISD::VNMSUBFP),// ->10957
/*10942*/   OPC_RecordChild0, // #0 = $A
/*10943*/   OPC_RecordChild1, // #1 = $B
/*10944*/   OPC_RecordChild2, // #2 = $C
/*10945*/   OPC_CheckType, MVT::v4f32,
/*10947*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::VNMSUBFP), 0,
                1/*#VTs*/, MVT::v4f32, 3/*#Ops*/, 0, 1, 2, 
            // Src: (PPCvnmsubfp:v4f32 VRRC:v4f32:$A, VRRC:v4f32:$B, VRRC:v4f32:$C) - Complexity = 3
            // Dst: (VNMSUBFP:v4f32 VRRC:v4f32:$A, VRRC:v4f32:$B, VRRC:v4f32:$C)
          /*SwitchOpcode*/ 7,  TARGET_OPCODE(PPCISD::NOP),// ->10967
/*10960*/   OPC_CaptureFlagInput,
/*10961*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::NOP), 0|OPFL_FlagInput|OPFL_FlagOutput,
                0/*#VTs*/, 0/*#Ops*/, 
            // Src: (PPCnop) - Complexity = 3
            // Dst: (NOP)
          /*SwitchOpcode*/ 13,  TARGET_OPCODE(PPCISD::LOAD_TOC),// ->10983
/*10970*/   OPC_RecordNode,   // #0 = 'PPCload_toc' chained node
/*10971*/   OPC_CaptureFlagInput,
/*10972*/   OPC_RecordChild1, // #1 = $reg
/*10973*/   OPC_CheckChild1Type, MVT::i64,
/*10975*/   OPC_EmitMergeInputChains1_0,
/*10976*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::LDinto_toc), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (PPCload_toc G8RC:i64:$reg) - Complexity = 3
            // Dst: (LDinto_toc G8RC:i64:$reg)
          /*SwitchOpcode*/ 9,  TARGET_OPCODE(PPCISD::TOC_RESTORE),// ->10995
/*10986*/   OPC_RecordNode,   // #0 = 'PPCtoc_restore' chained node
/*10987*/   OPC_CaptureFlagInput,
/*10988*/   OPC_EmitMergeInputChains1_0,
/*10989*/   OPC_MorphNodeTo, TARGET_OPCODE(PPC::LDtoc_restore), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput,
                0/*#VTs*/, 0/*#Ops*/, 
            // Src: (PPCtoc_restore) - Complexity = 3
            // Dst: (LDtoc_restore)
          0, // EndSwitchOpcode
    0
  }; // Total Array size is 10997 bytes

  // Opcode Histogram:
  // #OPC_Scope                          = 109
  // #OPC_RecordNode                     = 38
  // #OPC_RecordChild                    = 499
  // #OPC_RecordMemRef                   = 10
  // #OPC_CaptureFlagInput               = 18
  // #OPC_MoveChild                      = 148
  // #OPC_MoveParent                     = 367
  // #OPC_CheckSame                      = 0
  // #OPC_CheckPatternPredicate          = 21
  // #OPC_CheckPredicate                 = 179
  // #OPC_CheckOpcode                    = 95
  // #OPC_SwitchOpcode                   = 14
  // #OPC_CheckType                      = 153
  // #OPC_SwitchType                     = 94
  // #OPC_CheckChildType                 = 50
  // #OPC_CheckInteger                   = 191
  // #OPC_CheckCondCode                  = 0
  // #OPC_CheckValueType                 = 7
  // #OPC_CheckComplexPat                = 139
  // #OPC_CheckAndImm                    = 0
  // #OPC_CheckOrImm                     = 0
  // #OPC_CheckFoldableChainNode         = 0
  // #OPC_EmitInteger                    = 26
  // #OPC_EmitStringInteger              = 3
  // #OPC_EmitRegister                   = 1
  // #OPC_EmitConvertToTarget            = 78
  // #OPC_EmitMergeInputChains           = 184
  // #OPC_EmitCopyToReg                  = 0
  // #OPC_EmitNode                       = 8
  // #OPC_EmitNodeXForm                  = 38
  // #OPC_MarkFlagResults                = 0
  // #OPC_CompleteMatch                  = 12
  // #OPC_MorphNodeTo                    = 576

  #undef TARGET_OPCODE
  return SelectCodeCommon(N, MatcherTable,sizeof(MatcherTable));
}

bool CheckPatternPredicate(unsigned PredNo) const {
  switch (PredNo) {
  default: assert(0 && "Invalid predicate in table?");
  case 0: return (!NoExcessFPPrecision);
  case 1: return (!PPCSubTarget.isPPC64());
  case 2: return (PPCSubTarget.isPPC64());
  }
}

bool CheckNodePredicate(SDNode *Node, unsigned PredNo) const {
  switch (PredNo) {
  default: assert(0 && "Invalid predicate in table?");
  case 0: { // Predicate_immSExt16
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  // immSExt16 predicate - True if the immediate fits in a 16-bit sign extended
  // field.  Used by instructions like 'addi'.
  if (N->getValueType(0) == MVT::i32)
    return (int32_t)N->getZExtValue() == (short)N->getZExtValue();
  else
    return (int64_t)N->getZExtValue() == (short)N->getZExtValue();

  }
  case 1: { // Predicate_imm16ShiftedSExt
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  // imm16ShiftedSExt predicate - True if only bits in the top 16-bits of the
  // immediate are set.  Used by instructions like 'addis'.  Identical to 
  // imm16ShiftedZExt in 32-bit mode.
  if (N->getZExtValue() & 0xFFFF) return false;
  if (N->getValueType(0) == MVT::i32)
    return true;
  // For 64-bit, make sure it is sext right.
  return N->getZExtValue() == (uint64_t)(int)N->getZExtValue();

  }
  case 2: { // Predicate_unindexedload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getAddressingMode() == ISD::UNINDEXED;

  }
  case 3: { // Predicate_zextload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::ZEXTLOAD;

  }
  case 4: { // Predicate_zextloadi8
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 5: { // Predicate_sextload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::SEXTLOAD;

  }
  case 6: { // Predicate_sextloadi16
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 7: { // Predicate_zextloadi16
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 8: { // Predicate_load
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::NON_EXTLOAD;

  }
  case 9: { // Predicate_zextloadi1
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i1;

  }
  case 10: { // Predicate_extload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::EXTLOAD;

  }
  case 11: { // Predicate_extloadi1
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i1;

  }
  case 12: { // Predicate_extloadi8
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 13: { // Predicate_extloadi16
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 14: { // Predicate_sextloadi32
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i32;

  }
  case 15: { // Predicate_zextloadi32
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i32;

  }
  case 16: { // Predicate_extloadi32
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i32;

  }
  case 17: { // Predicate_extloadf32
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::f32;

  }
  case 18: { // Predicate_unindexedstore
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getAddressingMode() == ISD::UNINDEXED;

  }
  case 19: { // Predicate_truncstore
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->isTruncatingStore();

  }
  case 20: { // Predicate_truncstorei8
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 21: { // Predicate_truncstorei16
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 22: { // Predicate_store
    SDNode *N = Node;

  return !cast<StoreSDNode>(N)->isTruncatingStore();

  }
  case 23: { // Predicate_itruncstore
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->isTruncatingStore();

  }
  case 24: { // Predicate_pre_truncst
    SDNode *N = Node;

  ISD::MemIndexedMode AM = cast<StoreSDNode>(N)->getAddressingMode();
  return AM == ISD::PRE_INC || AM == ISD::PRE_DEC;

  }
  case 25: { // Predicate_pre_truncsti8
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 26: { // Predicate_pre_truncsti16
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 27: { // Predicate_istore
    SDNode *N = Node;

  return !cast<StoreSDNode>(N)->isTruncatingStore();

  }
  case 28: { // Predicate_pre_store
    SDNode *N = Node;

  ISD::MemIndexedMode AM = cast<StoreSDNode>(N)->getAddressingMode();
  return AM == ISD::PRE_INC || AM == ISD::PRE_DEC;

  }
  case 29: { // Predicate_truncstorei32
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i32;

  }
  case 30: { // Predicate_atomic_load_add_8
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i8;
  
  }
  case 31: { // Predicate_atomic_load_add_16
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i16;
  
  }
  case 32: { // Predicate_atomic_load_add_32
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;
  
  }
  case 33: { // Predicate_atomic_load_add_64
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;
  
  }
  case 34: { // Predicate_atomic_load_sub_8
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i8;
  
  }
  case 35: { // Predicate_atomic_load_sub_16
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i16;
  
  }
  case 36: { // Predicate_atomic_load_sub_32
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;
  
  }
  case 37: { // Predicate_atomic_load_sub_64
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;
  
  }
  case 38: { // Predicate_atomic_load_and_8
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i8;
  
  }
  case 39: { // Predicate_atomic_load_and_16
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i16;
  
  }
  case 40: { // Predicate_atomic_load_and_32
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;
  
  }
  case 41: { // Predicate_atomic_load_and_64
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;
  
  }
  case 42: { // Predicate_atomic_load_or_8
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i8;
  
  }
  case 43: { // Predicate_atomic_load_or_16
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i16;
  
  }
  case 44: { // Predicate_atomic_load_or_32
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;
  
  }
  case 45: { // Predicate_atomic_load_or_64
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;
  
  }
  case 46: { // Predicate_atomic_load_xor_8
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i8;
  
  }
  case 47: { // Predicate_atomic_load_xor_16
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i16;
  
  }
  case 48: { // Predicate_atomic_load_xor_32
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;
  
  }
  case 49: { // Predicate_atomic_load_xor_64
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;
  
  }
  case 50: { // Predicate_atomic_load_nand_8
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i8;
  
  }
  case 51: { // Predicate_atomic_load_nand_16
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i16;
  
  }
  case 52: { // Predicate_atomic_load_nand_32
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;
  
  }
  case 53: { // Predicate_atomic_load_nand_64
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;
  
  }
  case 54: { // Predicate_atomic_cmp_swap_8
    SDNode *N = Node;

  return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 55: { // Predicate_atomic_cmp_swap_16
    SDNode *N = Node;

  return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 56: { // Predicate_atomic_cmp_swap_32
    SDNode *N = Node;

  return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;

  }
  case 57: { // Predicate_atomic_cmp_swap_64
    SDNode *N = Node;

  return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;

  }
  case 58: { // Predicate_atomic_swap_8
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i8;
  
  }
  case 59: { // Predicate_atomic_swap_16
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i16;
  
  }
  case 60: { // Predicate_atomic_swap_32
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;
  
  }
  case 61: { // Predicate_atomic_swap_64
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;
  
  }
  case 62: { // Predicate_immZExt16
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  // immZExt16 predicate - True if the immediate fits in a 16-bit zero extended
  // field.  Used by instructions like 'ori'.
  return (uint64_t)N->getZExtValue() == (unsigned short)N->getZExtValue();

  }
  case 63: { // Predicate_imm16ShiftedZExt
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  // imm16ShiftedZExt predicate - True if only bits in the top 16-bits of the
  // immediate are set.  Used by instructions like 'xoris'.
  return (N->getZExtValue() & ~uint64_t(0xFFFF0000)) == 0;

  }
  case 64: { // Predicate_immAllOnesV
    SDNode *N = Node;

  return ISD::isBuildVectorAllOnes(N);

  }
  case 65: { // Predicate_maskimm32
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  // maskImm predicate - True if immediate is a run of ones.
  unsigned mb, me;
  if (N->getValueType(0) == MVT::i32)
    return isRunOfOnes((unsigned)N->getZExtValue(), mb, me);
  else
    return false;

  }
  case 66: { // Predicate_V_immneg0
    SDNode *N = Node;

  return PPC::isAllNegativeZeroVector(N);

  }
  case 67: { // Predicate_vspltb_shuffle
    SDNode *N = Node;

  return PPC::isSplatShuffleMask(cast<ShuffleVectorSDNode>(N), 1);

  }
  case 68: { // Predicate_vsplth_shuffle
    SDNode *N = Node;

  return PPC::isSplatShuffleMask(cast<ShuffleVectorSDNode>(N), 2);

  }
  case 69: { // Predicate_vspltw_shuffle
    SDNode *N = Node;

  return PPC::isSplatShuffleMask(cast<ShuffleVectorSDNode>(N), 4);

  }
  case 70: { // Predicate_vsldoi_unary_shuffle
    SDNode *N = Node;

  return PPC::isVSLDOIShuffleMask(N, true) != -1;

  }
  case 71: { // Predicate_vpkuwum_unary_shuffle
    SDNode *N = Node;

  return PPC::isVPKUWUMShuffleMask(cast<ShuffleVectorSDNode>(N), true);

  }
  case 72: { // Predicate_vpkuhum_unary_shuffle
    SDNode *N = Node;

  return PPC::isVPKUHUMShuffleMask(cast<ShuffleVectorSDNode>(N), true);

  }
  case 73: { // Predicate_vmrglb_unary_shuffle
    SDNode *N = Node;

  return PPC::isVMRGLShuffleMask(cast<ShuffleVectorSDNode>(N), 1, true);

  }
  case 74: { // Predicate_vmrglh_unary_shuffle
    SDNode *N = Node;

  return PPC::isVMRGLShuffleMask(cast<ShuffleVectorSDNode>(N), 2, true);

  }
  case 75: { // Predicate_vmrglw_unary_shuffle
    SDNode *N = Node;

  return PPC::isVMRGLShuffleMask(cast<ShuffleVectorSDNode>(N), 4, true);

  }
  case 76: { // Predicate_vmrghb_unary_shuffle
    SDNode *N = Node;

  return PPC::isVMRGHShuffleMask(cast<ShuffleVectorSDNode>(N), 1, true);

  }
  case 77: { // Predicate_vmrghh_unary_shuffle
    SDNode *N = Node;

  return PPC::isVMRGHShuffleMask(cast<ShuffleVectorSDNode>(N), 2, true);

  }
  case 78: { // Predicate_vmrghw_unary_shuffle
    SDNode *N = Node;

  return PPC::isVMRGHShuffleMask(cast<ShuffleVectorSDNode>(N), 4, true);

  }
  case 79: { // Predicate_vsldoi_shuffle
    SDNode *N = Node;

  return PPC::isVSLDOIShuffleMask(N, false) != -1;

  }
  case 80: { // Predicate_vmrghb_shuffle
    SDNode *N = Node;

  return PPC::isVMRGHShuffleMask(cast<ShuffleVectorSDNode>(N), 1, false);

  }
  case 81: { // Predicate_vmrghh_shuffle
    SDNode *N = Node;

  return PPC::isVMRGHShuffleMask(cast<ShuffleVectorSDNode>(N), 2, false);

  }
  case 82: { // Predicate_vmrghw_shuffle
    SDNode *N = Node;

  return PPC::isVMRGHShuffleMask(cast<ShuffleVectorSDNode>(N), 4, false);

  }
  case 83: { // Predicate_vmrglb_shuffle
    SDNode *N = Node;

  return PPC::isVMRGLShuffleMask(cast<ShuffleVectorSDNode>(N), 1, false);

  }
  case 84: { // Predicate_vmrglh_shuffle
    SDNode *N = Node;

  return PPC::isVMRGLShuffleMask(cast<ShuffleVectorSDNode>(N), 2, false);

  }
  case 85: { // Predicate_vmrglw_shuffle
    SDNode *N = Node;

  return PPC::isVMRGLShuffleMask(cast<ShuffleVectorSDNode>(N), 4, false);

  }
  case 86: { // Predicate_vpkuhum_shuffle
    SDNode *N = Node;

  return PPC::isVPKUHUMShuffleMask(cast<ShuffleVectorSDNode>(N), false);

  }
  case 87: { // Predicate_vpkuwum_shuffle
    SDNode *N = Node;

  return PPC::isVPKUWUMShuffleMask(cast<ShuffleVectorSDNode>(N), false);

  }
  case 88: { // Predicate_vecspltisb
    SDNode *N = Node;

  return PPC::get_VSPLTI_elt(N, 1, *CurDAG).getNode() != 0;

  }
  case 89: { // Predicate_vecspltish
    SDNode *N = Node;

  return PPC::get_VSPLTI_elt(N, 2, *CurDAG).getNode() != 0;

  }
  case 90: { // Predicate_vecspltisw
    SDNode *N = Node;

  return PPC::get_VSPLTI_elt(N, 4, *CurDAG).getNode() != 0;

  }
  case 91: { // Predicate_immAllZerosV
    SDNode *N = Node;

  return ISD::isBuildVectorAllZeros(N);

  }
  }
}

bool CheckComplexPattern(SDNode *Root, SDValue N,
      unsigned PatternNo, SmallVectorImpl<SDValue> &Result) {
  unsigned NextRes = Result.size();
  switch (PatternNo) {
  default: assert(0 && "Invalid pattern # in table?");
  case 0:
    Result.resize(NextRes+2);
    return SelectAddrIdxOnly(Root, N, Result[NextRes+0], Result[NextRes+1]);
  case 1:
    Result.resize(NextRes+2);
    return SelectAddrImm(Root, N, Result[NextRes+0], Result[NextRes+1]);
  case 2:
    Result.resize(NextRes+2);
    return SelectAddrIdx(Root, N, Result[NextRes+0], Result[NextRes+1]);
  case 3:
    Result.resize(NextRes+2);
    return SelectAddrImmShift(Root, N, Result[NextRes+0], Result[NextRes+1]);
  case 4:
    Result.resize(NextRes+1);
    return SelectAddrImmOffs(Root, N, Result[NextRes+0]);
  }
}

SDValue RunSDNodeXForm(SDValue V, unsigned XFormNo) {
  switch (XFormNo) {
  default: assert(0 && "Invalid xform # in table?");
  case 0: {  // HI16
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: shift the immediate value down into the low bits.
  return getI32Imm((unsigned)N->getZExtValue() >> 16);

  }
  case 1: {  // LO16
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: get the low 16 bits.
  return getI32Imm((unsigned short)N->getZExtValue());

  }
  case 2: {  // HA16
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: shift the immediate value down into the low bits.
  signed int Val = N->getZExtValue();
  return getI32Imm((Val - (signed short)Val) >> 16);

  }
  case 3: {  // MB
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: get the start bit of a mask
  unsigned mb = 0, me;
  (void)isRunOfOnes((unsigned)N->getZExtValue(), mb, me);
  return getI32Imm(mb);

  }
  case 4: {  // ME
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: get the end bit of a mask
  unsigned mb, me = 0;
  (void)isRunOfOnes((unsigned)N->getZExtValue(), mb, me);
  return getI32Imm(me);

  }
  case 5: {  // SHL32
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: 31 - imm
  return getI32Imm(31 - N->getZExtValue());

  }
  case 6: {  // SHL64
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: 63 - imm
  return getI32Imm(63 - N->getZExtValue());

  }
  case 7: {  // SRL32
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: 32 - imm
  return N->getZExtValue() ? getI32Imm(32 - N->getZExtValue()) : getI32Imm(0);

  }
  case 8: {  // SRL64
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: 64 - imm
  return N->getZExtValue() ? getI32Imm(64 - N->getZExtValue()) : getI32Imm(0);

  }
  case 9: {  // VSPLTB_get_imm
    SDNode *N = V.getNode();

  return getI32Imm(PPC::getVSPLTImmediate(N, 1));

  }
  case 10: {  // VSPLTH_get_imm
    SDNode *N = V.getNode();

  return getI32Imm(PPC::getVSPLTImmediate(N, 2));

  }
  case 11: {  // VSPLTW_get_imm
    SDNode *N = V.getNode();

  return getI32Imm(PPC::getVSPLTImmediate(N, 4));

  }
  case 12: {  // VSLDOI_unary_get_imm
    SDNode *N = V.getNode();

  return getI32Imm(PPC::isVSLDOIShuffleMask(N, true));

  }
  case 13: {  // VSLDOI_get_imm
    SDNode *N = V.getNode();

  return getI32Imm(PPC::isVSLDOIShuffleMask(N, false));

  }
  case 14: {  // VSPLTISB_get_imm
    SDNode *N = V.getNode();

  return PPC::get_VSPLTI_elt(N, 1, *CurDAG);

  }
  case 15: {  // VSPLTISH_get_imm
    SDNode *N = V.getNode();

  return PPC::get_VSPLTI_elt(N, 2, *CurDAG);

  }
  case 16: {  // VSPLTISW_get_imm
    SDNode *N = V.getNode();

  return PPC::get_VSPLTI_elt(N, 4, *CurDAG);

  }
  }
}

