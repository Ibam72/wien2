#include <DxLib.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

int WINAPI WinMain(HINSTANCE,HINSTANCE,LPSTR,int){
	ChangeWindowMode(TRUE),SetGraphMode(1200,600,32),DxLib_Init(), SetDrawScreen( DX_SCREEN_BACK );

	

	//キャラ、カメラ変数 CA-カメラ　CH-キャラ CHR1-キャラの向き
	int chr;
	int ani;
	int anime;
	chr = 0;
	VECTOR CAVect,CHRVect,CHVect,CATVect,CHR1Vect,beforeCHVect;
	CHVect = VGet(0.0f,0.0f,0.0f);
	CAVect = VGet(0.0f,10.0f,0.0f);
	CATVect = VGet(0.0f,10.0f,0.0f);
	CHRVect = VGet(0.0f,0.0f,0.0f);
	float animetime,PlayTime = 0.0f;
	int MYModel = MV1LoadModel( "dat/Lat式ミク/Lat式ミクVer2.3_Normal.pmd" );
	anime = MV1AttachAnim( MYModel,0,-1,FALSE);
	MV1SetScale( MYModel, VGet(0.1f,0.1f,0.1f) );
	
	SetCameraNearFar( 0.1f, 1000.0f ) ;
	SetCameraPositionAndTarget_UpVecY( CAVect, CATVect ) ;

	



	//stage変数
	int StageModel = MV1LoadModel("dat/バトーキン島/batokin_island5.x");
	MV1SetupCollInfo( StageModel,-1,32,8,32);
	MV1_COLL_RESULT_POLY HitPoly ;

	//enemymodel
	int Model1 = MV1DuplicateModel(MYModel);
	VECTOR E1Vect;
	//位置決定
	int r1,r2;
	srand(100);
	r1 = rand()%100-49;
	r2 = rand()%100-49;
	E1Vect = VGet(r1,0,r2);

	HitPoly =MV1CollCheck_Line( StageModel, -1, VGet(E1Vect.x,100.0f,E1Vect.z),VGet(E1Vect.x, - 100.0f,E1Vect.z) );
		if( HitPoly.HitFlag == 1)
		{
			E1Vect = HitPoly.HitPosition;
		}
	//当たり判定
	MV1SetupCollInfo( Model1, -1, 8, 8, 8 ) ;
	MV1SetupCollInfo( MYModel, -1, 8, 8, 8 ) ;
	MV1_COLL_RESULT_POLY_DIM HitPolyDim ;




	//演算用変数
	VECTOR MVect1,MVect2;
	int i,j,k,l;
	i = 0;
	k=0;
	j=0;
	l=0;

	

	//自弾[値][弾番号]
	int sphere[2][12];
	int sflag,counter;
		sflag=0;
	VECTOR SVect[2][13];

	//敵弾[値][弾番号]
	VECTOR EVect[2][13];
	int h;
	//自HP
	int HP;
	HP=-1;
	//敵HP
	int EHP;
	EHP=10;


	//フレームカウンター
	int frame=0;


	while(!ScreenFlip()&&!ProcessMessage()&&!ClearDrawScreen()){

		if(frame>=60)
			frame =frame-59;
		else
			frame =++frame;

        // 初期化&インクリメント演算処理
		PlayTime += 0.8f;
		ani = 0;
		chr = 0;
		beforeCHVect = CHVect;
		//判定表示
		MV1RefreshCollInfo( Model1,-1 ) ;
		MV1RefreshCollInfo( MYModel,-1 );



		//歩行
        MVect1 = VSub(CHVect,CAVect);
        MVect1 = VSub(MVect1,VGet(0.0f,MVect1.y,0.0f));
        MVect1 = VScale(VNorm( MVect1 ),0.2f);
        MVect2 = VGet(MVect1.z*(-1.0f), 0,MVect1.x);

        
		if(CheckHitKey(KEY_INPUT_W)){
			CHVect = VAdd(CHVect,MVect1);
			ani=ani+1;
			chr=1;
		}
		if(CheckHitKey(KEY_INPUT_S)){
			CHVect = VSub(CHVect,MVect1);
			ani=ani-1;
			chr=chr+2;
		}
		if(CheckHitKey(KEY_INPUT_A)){
			CHVect = VAdd(CHVect,MVect2);
			ani=ani+2;
			chr=chr+4;
		}
		if(CheckHitKey(KEY_INPUT_D)){
			CHVect = VSub(CHVect,MVect2);
			ani=ani-2;
			chr=chr+8;
		}

		//stage衝突判定
		HitPoly =MV1CollCheck_Line( StageModel, -1, VGet(CHVect.x,CHVect.y + 1.0f,CHVect.z),VGet(CHVect.x, CHVect.y - 1.0f,CHVect.z) );
		if( HitPoly.HitFlag == 1)
		{
			CHVect = HitPoly.HitPosition;
			k=0;
		}
		if( HitPoly.HitFlag == 0)
			CHVect = beforeCHVect;

		//キャラクターの向き
		if((chr == 1)|(chr == 13))
			CHRVect=VGet(0.0f,0.0f,0.0f);
		else if((chr == 2)|(chr == 14))
			CHRVect=VGet(0.0f,DX_PI_F,0.0f);
		else if((chr == 4)|(chr ==7))
			CHRVect=VGet(0.0f,DX_PI_F/2.0f*3.0f,0.0f);
		else if(chr == 5)
			CHRVect=VGet(0.0f,DX_PI_F/4.0f*7.0f,0.0f);
		else if(chr == 6)
			CHRVect=VGet(0.0f,DX_PI_F/4.0f*5.0f,0.0f);
		else if((chr == 8)|(chr == 11))
			CHRVect = VGet(0.0f,DX_PI_F/2.0f,0.0f);
		else if(chr == 9)
			CHRVect=VGet(0.0f,-1.0f*DX_PI_F/4.0*7.0f,0.0f);
		else if(chr == 10)
			CHRVect=VGet(0.0f,-1.0f*DX_PI_F/4.0*5.0f,0.0f);

		CHR1Vect = VSub(CHRVect,VGet(0.0f,DX_PI_F*(i-30)/60,0.0f));

		//カメラ
		if( CheckHitKey( KEY_INPUT_LEFT ) == 1)
			++i;
		if( CheckHitKey( KEY_INPUT_RIGHT ) == 1)
			--i;
		
		CAVect = VGet(5*cosf(DX_PI_F*i/60),3.0f,5*sinf(DX_PI_F*i/60));
		CAVect = VAdd(CAVect,CHVect);
		CATVect = VAdd(CHVect,VGet(0.0f,2.0f,0.0f));

		//自弾設定
		if(CheckHitKey(KEY_INPUT_T)){
			if(sflag==0){
				sflag=1;
				counter=frame%5;
			}
			if(frame%5==counter){
				l=(frame-frame%5)/5;
				sphere[0][l]=1;
				SVect[0][l]=VAdd(CHVect,VGet(0,2,0));
				SVect[1][l]=VScale(MVect1,3.0f);
				
			}
		}
		else if(sflag==1){
			sflag=0;}

		for (j=0;j<12;j++){
			if(sphere[0][j]==1){
				//位置設定
				SVect[0][j]=VAdd(SVect[1][j],SVect[0][j]);
				//当たり判定
				HitPolyDim = MV1CollCheck_Sphere( Model1,-1,SVect[0][j], 2.0f );
				if(HitPolyDim.HitNum>=1){
					sphere[0][j]=0;
					--EHP;
				}
				MV1CollResultPolyDimTerminate( HitPolyDim );
			}
		}
		//敵弾設定
		if(frame%5==0){
			h=frame/5;
			EVect[0][h]=E1Vect;
			EVect[1][h]=VScale(VNorm(VSub(CHVect,E1Vect)),3.0f);
		}
		for(j=0;j<12;j++){
			EVect[0][j]=VAdd(EVect[0][j],EVect[1][j]);
			HitPolyDim = MV1CollCheck_Sphere( MYModel,-1,EVect[0][j], 2.0f );
				if(HitPolyDim.HitNum>=1){
					--HP;
				}
				MV1CollResultPolyDimTerminate( HitPolyDim );
		}



		//animetion
		if(ani==0){
			MV1DetachAnim(MYModel,anime);
            anime = MV1AttachAnim( MYModel,0,-1,FALSE);
			animetime = MV1GetAttachAnimTotalTime(MYModel,anime);
		}
		else if (ani==16){
			MV1DetachAnim(MYModel,anime);
            anime = MV1AttachAnim( MYModel,4,-1,FALSE);
			animetime = MV1GetAttachAnimTotalTime(MYModel,anime);
	    }
		else{
			MV1DetachAnim(MYModel,anime);
            anime = MV1AttachAnim( MYModel,1,-1,FALSE);
			animetime = MV1GetAttachAnimTotalTime(MYModel,anime);
		}
		MV1SetAttachAnimTime( MYModel, anime, PlayTime );

		if(PlayTime >=animetime){
			PlayTime = 0.0f;
		}
		//判定削除

		// ３Ｄモデル表示
		
		//キャラ表示
		SetCameraPositionAndTarget_UpVecY(CAVect , CATVect ) ;
		MV1SetRotationXYZ(MYModel,CHR1Vect);
		MV1SetPosition(MYModel,CHVect);
		MV1SetPosition(Model1,E1Vect);
		MV1DrawModel( MYModel ) ;
		MV1DrawModel(StageModel);
		MV1DrawModel(Model1);
		//自敵弾表示
			for (j=0;j<12;j++){
				if(sphere[0][j]==1){
			DrawSphere3D( SVect[0][j],1.0f,16,GetColor(255,0,0),GetColor(0,0,0), FALSE );
				}
				DrawSphere3D( EVect[0][j],1.0f,16,GetColor(255,0,0),GetColor(0,0,0), FALSE );
			}
			
		//文字表示
		DrawFormatString( 0, 0, GetColor(255,255,255),"体力( %d )",HP ) ;
		
		//ゲーム終了
		if(EHP<=0)
			DxLib_End();

		

	}

	DxLib_End();
	return 0;
}