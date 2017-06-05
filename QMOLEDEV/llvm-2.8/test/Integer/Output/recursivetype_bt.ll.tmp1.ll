; ModuleID = '<stdin>'

%list = type { %list*, i36 }

declare i8* @malloc(i32)

define void @InsertIntoListTail(%list** %L, i36 %Data) {
bb1:
  %reg116 = load %list** %L
  %cast1004 = inttoptr i64 0 to %list*
  %cond1000 = icmp eq %list* %reg116, %cast1004
  br i1 %cond1000, label %bb3, label %bb2

bb2:                                              ; preds = %bb2, %bb1
  %reg117 = phi %list** [ %reg118, %bb2 ], [ %L, %bb1 ]
  %cast1010 = bitcast %list** %reg117 to %list***
  %reg118 = load %list*** %cast1010
  %reg109 = load %list** %reg118
  %cast1005 = inttoptr i64 0 to %list*
  %cond1001 = icmp ne %list* %reg109, %cast1005
  br i1 %cond1001, label %bb2, label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %reg119 = phi %list** [ %reg118, %bb2 ], [ %L, %bb1 ]
  %cast1006 = bitcast %list** %reg119 to i8**
  %reg111 = call i8* @malloc(i32 16)
  store i8* %reg111, i8** %cast1006
  %reg112 = ptrtoint i8* %reg111 to i64
  %reg1002 = add i64 %reg112, 8
  %reg1005 = inttoptr i64 %reg1002 to i8*
  %cast1008 = bitcast i8* %reg1005 to i36*
  store i36 %Data, i36* %cast1008
  %cast1003 = inttoptr i64 0 to i64*
  %cast1009 = bitcast i8* %reg111 to i64**
  store i64* %cast1003, i64** %cast1009
  ret void
}

define %list* @FindData(%list* %L, i36 %Data) {
bb1:
  br label %bb2

bb2:                                              ; preds = %bb6, %bb1
  %reg115 = phi %list* [ %reg116, %bb6 ], [ %L, %bb1 ]
  %cast1014 = inttoptr i64 0 to %list*
  %cond1011 = icmp ne %list* %reg115, %cast1014
  br i1 %cond1011, label %bb4, label %bb3

bb3:                                              ; preds = %bb2
  ret %list* null

bb4:                                              ; preds = %bb2
  %idx = getelementptr %list* %reg115, i64 0, i32 1
  %reg111 = load i36* %idx
  %cond1013 = icmp ne i36 %reg111, %Data
  br i1 %cond1013, label %bb6, label %bb5

bb5:                                              ; preds = %bb4
  ret %list* %reg115

bb6:                                              ; preds = %bb4
  %idx2 = getelementptr %list* %reg115, i64 0, i32 0
  %reg116 = load %list** %idx2
  br label %bb2
}
