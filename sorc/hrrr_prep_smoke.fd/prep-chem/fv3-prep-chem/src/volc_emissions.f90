!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!
module volcanoes_emissions
! ------------------------------------------------
! Indices para os dados da matriz emission-factors
! ------------------------------------------------
! Coluna - 
! ------------------------------------------------
 integer,parameter :: &
       Latitude       = 01, & ! Latitude
       Longitude      = 02, & ! longitude
       Elevation      = 03, & ! Elevation
       TimeFrame      = 04, & ! TimeFrame
       ESP            = 05, & ! ESP (Eruption Type)
       SRC_ON         = 06    ! turn on/off the source from this vulcan
 integer, parameter :: on=1, off=0
! ------------------------------------------------
integer, parameter :: nvolcanoes=1535
character(LEN=30),dimension(nvolcanoes),parameter :: volcanoes_name= &
! avoid tab character
! '123456789012345678901234567890'
(/                                &
  'WESTEIFELVOLCFIELD            '&!            1
 ,'CHAINEDESPUYS                 '&!            2
 ,'OLOTVOLCFIELD                 '&!            3
 ,'CALATRAVAVOLCFIELD            '&!            4
 ,'LARDERELLO                    '&!            5
 ,'VULSINI                       '&!            6
 ,'ALBANHILLS                    '&!            7
 ,'CAMPIFLEGREI                  '&!            8
 ,'VESUVIUS                      '&!            9
 ,'ISCHIA                        '&!           10
 ,'PANAREA                       '&!           11
 ,'LIPARI                        '&!           12
 ,'STROMBOLI                     '&!           13
 ,'VULCANO                       '&!           14
 ,'ETNA                          '&!           15
 ,'PANTELLERIA                   '&!           16
 ,'CAMPIFLEGREIMARSICILIA        '&!           17
 ,'METHANA                       '&!           18
 ,'MILOS                         '&!           19
 ,'SANTORINI                     '&!           20
 ,'YALI                          '&!           21
 ,'NISYROS                       '&!           22
 ,'KULA                          '&!           23
 ,'KARAPINARFIELD                '&!           24
 ,'HASANDAGI                     '&!           25
 ,'GOLLUDAG                      '&!           26
 ,'ACIGOLNEVSEHIR                '&!           27
 ,'KARACADAG                     '&!           28
 ,'ERCIYESDAGI                   '&!           29
 ,'SUPHANDAGI                    '&!           30
 ,'GIREKOL                       '&!           31
 ,'NEMRUTDAGI                    '&!           32
 ,'TENDUREKDAGI                  '&!           33
 ,'ARARAT                        '&!           34
 ,'KARSPLATEAU                   '&!           35
 ,'ELBRUS                        '&!           36
 ,'KASBEK                        '&!           37
 ,'KABARGINOTHGROUP              '&!           38
 ,'UNNAMED                       '&!           39
 ,'UNNAMED                       '&!           40
 ,'ARAGATS                       '&!           41
 ,'GHEGAMRIDGE                   '&!           42
 ,'DARALAGES                     '&!           43
 ,'PORAK                         '&!           44
 ,'TSKHOUKKARCKAR                '&!           45
 ,'TAIRJEBELAT                   '&!           46
 ,'ZUKUR                         '&!           47
 ,'HANISH                        '&!           48
 ,'ZUBAIRJEBEL                   '&!           49
 ,'JALUA                         '&!           50
 ,'DALLOL                        '&!           51
 ,'ALID                          '&!           52
 ,'GADAALE                       '&!           53
 ,'ALU                           '&!           54
 ,'BORALEALE                     '&!           55
 ,'DALAFFILLA                    '&!           56
 ,'ERTAALE                       '&!           57
 ,'HAYLIGUBBI                    '&!           58
 ,'ALEBAGU                       '&!           59
 ,'NABRO                         '&!           60
 ,'MALLAHLE                      '&!           61
 ,'SORKALE                       '&!           62
 ,'ASAVYO                        '&!           63
 ,'MATALA                        '&!           64
 ,'TATALI                        '&!           65
 ,'BORAWLI                       '&!           66
 ,'DUBBI                         '&!           67
 ,'MAALALTA                      '&!           68
 ,'ALAYTA                        '&!           69
 ,'DABBAHU                       '&!           70
 ,'DABBAYRA                      '&!           71
 ,'MANDAHARARO                   '&!           72
 ,'GROPPO                        '&!           73
 ,'AFDERA                        '&!           74
 ,'BORAWLI                       '&!           75
 ,'MANDAINAKIR                   '&!           76
 ,'MOUSAALLI                     '&!           77
 ,'GUFA                          '&!           78
 ,'ASSABVOLCFIELD                '&!           79
 ,'ARDOUKOBA                     '&!           80
 ,'KURUB                         '&!           81
 ,'DAMAALI                       '&!           82
 ,'YANGUDI                       '&!           83
 ,'GABILLEMA                     '&!           84
 ,'AYELU                         '&!           85
 ,'HERTALI                       '&!           86
 ,'LIADOHAYK                     '&!           87
 ,'ADWA                          '&!           88
 ,'DOFEN                         '&!           89
 ,'BERU                          '&!           90
 ,'FENTALE                       '&!           91
 ,'KONE                          '&!           92
 ,'UNNAMED                       '&!           93
 ,'BOSETBERICHA                  '&!           94
 ,'BISHOFTUVOLCFIELD             '&!           95
 ,'UNNAMED                       '&!           96
 ,'SODORE                        '&!           97
 ,'GEDAMSACALDERA                '&!           98
 ,'BORABERICCIO                  '&!           99
 ,'TULLUMOJE                     '&!          100
 ,'UNNAMED                       '&!          101
 ,'EASTZWAY                      '&!          102
 ,'BUTAJIRISILTIFIELD            '&!          103
 ,'ALUTU                         '&!          104
 ,'OACALDERA                     '&!          105
 ,'CORBETTICALDERA               '&!          106
 ,'BILATERIVERFIELD              '&!          107
 ,'TEPI                          '&!          108
 ,'HOBICHACALDERA                '&!          109
 ,'CHIRACHA                      '&!          110
 ,'TOSASUCHA                     '&!          111
 ,'UNNAMED                       '&!          112
 ,'KORATHRANGE                   '&!          113
 ,'MEGABASALTFIELD               '&!          114
 ,'NORTHISLAND                   '&!          115
 ,'CENTRALISLAND                 '&!          116
 ,'MARSABIT                      '&!          117
 ,'SOUTHISLAND                   '&!          118
 ,'BARRIERTHE                    '&!          119
 ,'NAMARUNU                      '&!          120
 ,'SEGERERUAPLATEAU              '&!          121
 ,'EMURUANGOGOLAK                '&!          122
 ,'SILALI                        '&!          123
 ,'PAKA                          '&!          124
 ,'KOROSI                        '&!          125
 ,'OLKOKWE                       '&!          126
 ,'NYAMBENIHILLS                 '&!          127
 ,'MENENGAI                      '&!          128
 ,'ELMENTEITABADLANDS            '&!          129
 ,'HOMAMOUNTAIN                  '&!          130
 ,'EBURRUOLDOINYO                '&!          131
 ,'OLKARIA                       '&!          132
 ,'LONGONOT                      '&!          133
 ,'SUSWA                         '&!          134
 ,'LENGAIOLDOINYO                '&!          135
 ,'CHYULUHILLS                   '&!          136
 ,'KILIMANJARO                   '&!          137
 ,'IGWISIHILLS                   '&!          138
 ,'UNNAMED                       '&!          139
 ,'SWUSANGUBASIN                 '&!          140
 ,'NGOZI                         '&!          141
 ,'IZUMBWEMPOLI                  '&!          142
 ,'RUNGWE                        '&!          143
 ,'MERU                          '&!          144
 ,'KIEYO                         '&!          145
 ,'FORTPORTAL                    '&!          146
 ,'KYATWA                        '&!          147
 ,'KATWEKIKORONGO                '&!          148
 ,'BUNYARUGURU                   '&!          149
 ,'KATUNGA                       '&!          150
 ,'MAYYAMOTO                     '&!          151
 ,'NYAMURAGIRA                   '&!          152
 ,'NYIRAGONGO                    '&!          153
 ,'KARISIMBI                     '&!          154
 ,'VISOKE                        '&!          155
 ,'MUHAVURA                      '&!          156
 ,'BUFUMBIRA                     '&!          157
 ,'TSHIBINDA                     '&!          158
 ,'SAOTOME                       '&!          159
 ,'SANCARLOS                     '&!          160
 ,'SANJOAQUIN                    '&!          161
 ,'SANTAISABEL                   '&!          162
 ,'TOMBELGRABEN                  '&!          163
 ,'CAMEROON                      '&!          164
 ,'MANENGOUBA                    '&!          165
 ,'OKUVOLCFIELD                  '&!          166
 ,'NGAOUNDEREPLATEAU             '&!          167
 ,'BIUPLATEAU                    '&!          168
 ,'TODRAVOLCFIELD                '&!          169
 ,'TINZAOUATENEVOLCFIELD         '&!          170
 ,'INEZZANEVOLCFIELD             '&!          171
 ,'TAHALRAVOLCFIELD              '&!          172
 ,'ATAKORVOLCFIELD               '&!          173
 ,'MANZAZVOLCFIELD               '&!          174
 ,'HARUJ                         '&!          175
 ,'WAUENNAMUS                    '&!          176
 ,'TOHTARSO                      '&!          177
 ,'TOUSSIDETARSO                 '&!          178
 ,'KOUSSIEMI                     '&!          179
 ,'VOONTARSO                     '&!          180
 ,'MARRAJEBEL                    '&!          181
 ,'KUTUMVOLCFIELD                '&!          182
 ,'MEIDOBVOLCFIELD               '&!          183
 ,'BAYUDAVOLCFIELD               '&!          184
 ,'UMMARAFIEBJEBEL               '&!          185
 ,'SHARATKOVAKAB                 '&!          186
 ,'UNNAMED                       '&!          187
 ,'GOLANHEIGHTS                  '&!          188
 ,'UNNAMED                       '&!          189
 ,'ESSAFA                        '&!          190
 ,'DRUZEJABALAD                  '&!          191
 ,'HARRAHAL                      '&!          192
 ,'RAHAHHARRATAR                 '&!          193
 ,'UWAYRIDHARRAT                 '&!          194
 ,'LUNAYYIRHARRAT                '&!          195
 ,'ITHNAYNHARRAT                 '&!          196
 ,'KHAYBARHARRAT                 '&!          197
 ,'KISHBHARRAT                   '&!          198
 ,'BIRKHARRATAL                  '&!          199
 ,'RAHATHARRAT                   '&!          200
 ,'YARJABAL                      '&!          201
 ,'ARHABHARRAOF                  '&!          202
 ,'MARHAJABALEL                  '&!          203
 ,'HAYLANJABAL                   '&!          204
 ,'DHAMARHARRASOF                '&!          205
 ,'UNNAMED                       '&!          206
 ,'SAWADHARRAES                  '&!          207
 ,'BALHAFHARRAOF                 '&!          208
 ,'BIRBORHUT                     '&!          209
 ,'UNNAMED                       '&!          210
 ,'SAHAND                        '&!          211
 ,'SABALAN                       '&!          212
 ,'DAMAVAND                      '&!          213
 ,'QALEHHASANALI                 '&!          214
 ,'BAZMAN                        '&!          215
 ,'UNNAMED                       '&!          216
 ,'TAFTAN                        '&!          217
 ,'DACHTINAVARGROUP              '&!          218
 ,'VAKAKGROUP                    '&!          219
 ,'GRILLELA                      '&!          220
 ,'AMBREBOBAOMBY                 '&!          221
 ,'NOSYBE                        '&!          222
 ,'ANKAIZINAFIELD                '&!          223
 ,'ITASYVOLCFIELD                '&!          224
 ,'ANKARATRAFIELD                '&!          225
 ,'KARTHALA                      '&!          226
 ,'FOURNAISEPITONDELA            '&!          227
 ,'BOOMERANGSEAMOUNT             '&!          228
 ,'AMSTERDAMISLAND               '&!          229
 ,'ST.PAUL                       '&!          230
 ,'MCDONALDISLANDS               '&!          231
 ,'HEARD                         '&!          232
 ,'KERGUELENISLANDS              '&!          233
 ,'ESTILEDEL                     '&!          234
 ,'POSSESSIONILEDELA             '&!          235
 ,'COCHONSILEAUX                 '&!          236
 ,'PRINCEEDWARDISLAND            '&!          237
 ,'MARIONISLAND                  '&!          238
 ,'UNNAMED                       '&!          239
 ,'WHANGAREI                     '&!          240
 ,'KAIKOHEBAYOFISLANDS           '&!          241
 ,'MAYORISLAND                   '&!          242
 ,'AUCKLANDFIELD                 '&!          243
 ,'TARANAKI[EGMONT]              '&!          244
 ,'WHITEISLAND                   '&!          245
 ,'OKATAINA                      '&!          246
 ,'REPOROA                       '&!          247
 ,'MAROA                         '&!          248
 ,'TAUPO                         '&!          249
 ,'TONGARIRO                     '&!          250
 ,'CLARK                         '&!          251
 ,'TANGAROA                      '&!          252
 ,'RUAPEHU                       '&!          253
 ,'RUMBLEV                       '&!          254
 ,'RUMBLEIV                      '&!          255
 ,'RUMBLEIII                     '&!          256
 ,'RUMBLEIIWEST                  '&!          257
 ,'HEALY                         '&!          258
 ,'BROTHERS                      '&!          259
 ,'VOLCANOW                      '&!          260
 ,'CURTISISLAND                  '&!          261
 ,'MACAULEYISLAND                '&!          262
 ,'GIGGENBACH                    '&!          263
 ,'RAOULISLAND                   '&!          264
 ,'MONOWAISEAMOUNT               '&!          265
 ,'UNNAMED                       '&!          266
 ,'UNNAMED                       '&!          267
 ,'UNNAMED                       '&!          268
 ,'UNNAMED                       '&!          269
 ,'HUNGATONGAHUNGAHAAPAI         '&!          270
 ,'FALCONISLAND                  '&!          271
 ,'KAO                           '&!          272
 ,'TOFUA                         '&!          273
 ,'METISSHOAL                    '&!          274
 ,'HOMEREEF                      '&!          275
 ,'UNNAMED                       '&!          276
 ,'LATE                          '&!          277
 ,'TAFAHI                        '&!          278
 ,'CURACOA                       '&!          279
 ,'FONUALEI                      '&!          280
 ,'NIUAFOOU                      '&!          281
 ,'VAILULUU                      '&!          282
 ,'TAU                           '&!          283
 ,'OFUOLOSEGA                    '&!          284
 ,'TUTUILA                       '&!          285
 ,'UPOLU                         '&!          286
 ,'SAVAII                        '&!          287
 ,'WALLISISLANDS                 '&!          288
 ,'TAVEUNI                       '&!          289
 ,'KORO                          '&!          290
 ,'NABUKELEVU                    '&!          291
 ,'STANDREWSTRAIT                '&!          292
 ,'BALUAN                        '&!          293
 ,'UNNAMED                       '&!          294
 ,'BLUPBLUP                      '&!          295
 ,'KADOVAR                       '&!          296
 ,'BOISA                         '&!          297
 ,'BAM                           '&!          298
 ,'MANAM                         '&!          299
 ,'KARKAR                        '&!          300
 ,'YOMBA                         '&!          301
 ,'UNNAMED                       '&!          302
 ,'LONGISLAND                    '&!          303
 ,'UMBOI                         '&!          304
 ,'RITTERISLAND                  '&!          305
 ,'SAKAR                         '&!          306
 ,'UNNAMED                       '&!          307
 ,'LANGILA                       '&!          308
 ,'MUNDUA                        '&!          309
 ,'GAROVE                        '&!          310
 ,'DAKATAUA                      '&!          311
 ,'BOLA                          '&!          312
 ,'GARUAHARBOUR                  '&!          313
 ,'LOLO                          '&!          314
 ,'GARBUNAGROUP                  '&!          315
 ,'PAGO                          '&!          316
 ,'SULURANGE                     '&!          317
 ,'HARGY                         '&!          318
 ,'BAMUS                         '&!          319
 ,'ULAWUN                        '&!          320
 ,'UNNAMED                       '&!          321
 ,'LOLOBAU                       '&!          322
 ,'RABAUL                        '&!          323
 ,'TAVUI                         '&!          324
 ,'DOMAPEAKS                     '&!          325
 ,'CRATERMOUNTAIN                '&!          326
 ,'YELIA                         '&!          327
 ,'KORANGA                       '&!          328
 ,'MADILOGO                      '&!          329
 ,'HYDROGRAPHERSRANGE            '&!          330
 ,'LAMINGTON                     '&!          331
 ,'MANAGLASEPLATEAU              '&!          332
 ,'MUSARIVER                     '&!          333
 ,'SESSAGARA                     '&!          334
 ,'VICTORY                       '&!          335
 ,'GOODENOUGH                    '&!          336
 ,'WAIOWA                        '&!          337
 ,'IAMALELE                      '&!          338
 ,'DAWSONSTRAITGROUP             '&!          339
 ,'LIHIR                         '&!          340
 ,'AMBITLE                       '&!          341
 ,'TORE                          '&!          342
 ,'BILLYMITCHELL                 '&!          343
 ,'BALBI                         '&!          344
 ,'TAKUANGROUP                   '&!          345
 ,'BAGANA                        '&!          346
 ,'LOLORU                        '&!          347
 ,'KANAKEOKI                     '&!          348
 ,'COLEMANSEAMOUNT               '&!          349
 ,'SIMBO                         '&!          350
 ,'UNNAMED                       '&!          351
 ,'GALLEGO                       '&!          352
 ,'KAVACHI                       '&!          353
 ,'SAVO                          '&!          354
 ,'TINAKULA                      '&!          355
 ,'MOTLAV                        '&!          356
 ,'SURETAMATAI                   '&!          357
 ,'MERELAVA                      '&!          358
 ,'GAUA                          '&!          359
 ,'AOBA                          '&!          360
 ,'AMBRYM                        '&!          361
 ,'LOPEVI                        '&!          362
 ,'EPI                           '&!          363
 ,'KUWAE                         '&!          364
 ,'UNNAMED                       '&!          365
 ,'NORTHVATE                     '&!          366
 ,'TRAITORSHEAD                  '&!          367
 ,'YASUR                         '&!          368
 ,'ANEITYUM                      '&!          369
 ,'EASTERNGEMINISEAMOUNT         '&!          370
 ,'MATTHEWISLAND                 '&!          371
 ,'HUNTERISLAND                  '&!          372
 ,'UNNAMED                       '&!          373
 ,'NEWERVOLCANICSPROV            '&!          374
 ,'NARCONDUM                     '&!          375
 ,'BARRENISLAND                  '&!          376
 ,'SEULAWAHAGAM                  '&!          377
 ,'PEUETSAGUE                    '&!          378
 ,'TELONGBURNI                   '&!          379
 ,'SIBAYAK                       '&!          380
 ,'SINABUNG                      '&!          381
 ,'TOBA                          '&!          382
 ,'IMUN                          '&!          383
 ,'LUBUKRAYA                     '&!          384
 ,'SIBUALBUALI                   '&!          385
 ,'SORIKMARAPI                   '&!          386
 ,'SARIKGAJAH                    '&!          387
 ,'TALAKMAU                      '&!          388
 ,'MARAPI                        '&!          389
 ,'TANDIKAT                      '&!          390
 ,'TALANG                        '&!          391
 ,'KUNYIT                        '&!          392
 ,'HUTAPANJANG                   '&!          393
 ,'KERINCI                       '&!          394
 ,'SUMBING                       '&!          395
 ,'PENDAN                        '&!          396
 ,'BELIRANGBERITI                '&!          397
 ,'DAUNBUKIT                     '&!          398
 ,'KABA                          '&!          399
 ,'PATAH                         '&!          400
 ,'DEMPO                         '&!          401
 ,'LUMUTBALAIBUKIT               '&!          402
 ,'RANAU                         '&!          403
 ,'BESAR                         '&!          404
 ,'SEKINCAUBELIRANG              '&!          405
 ,'SUOH                          '&!          406
 ,'HULUBELU                      '&!          407
 ,'RAJABASA                      '&!          408
 ,'KRAKATAU                      '&!          409
 ,'PULOSARI                      '&!          410
 ,'KARANG                        '&!          411
 ,'PERBAKTIGAGAK                 '&!          412
 ,'SALAK                         '&!          413
 ,'GEDE                          '&!          414
 ,'PATUHA                        '&!          415
 ,'MALABAR                       '&!          416
 ,'WAYANGWINDU                   '&!          417
 ,'TANGKUBANPARAHU               '&!          418
 ,'PAPANDAYAN                    '&!          419
 ,'KENDANG                       '&!          420
 ,'TAMPOMAS                      '&!          421
 ,'GUNTUR                        '&!          422
 ,'GALUNGGUNG                    '&!          423
 ,'TALAGABODAS                   '&!          424
 ,'KARAHAKAWAH                   '&!          425
 ,'CEREME                        '&!          426
 ,'SLAMET                        '&!          427
 ,'DIENGVOLCCOMPLEX              '&!          428
 ,'SUNDORO                       '&!          429
 ,'SUMBING                       '&!          430
 ,'TELOMOYO                      '&!          431
 ,'UNGARAN                       '&!          432
 ,'MERBABU                       '&!          433
 ,'MURIA                         '&!          434
 ,'MERAPI                        '&!          435
 ,'LAWU                          '&!          436
 ,'WILIS                         '&!          437
 ,'KAWIBUTAK                     '&!          438
 ,'KELUT                         '&!          439
 ,'PENANGGUNGAN                  '&!          440
 ,'MALANGPLAIN                   '&!          441
 ,'ARJUNOWELIRANG                '&!          442
 ,'SEMERU                        '&!          443
 ,'TENGGERCALDERA                '&!          444
 ,'LURUS                         '&!          445
 ,'LAMONGAN                      '&!          446
 ,'IYANGARGAPURA                 '&!          447
 ,'RAUNG                         '&!          448
 ,'BALURAN                       '&!          449
 ,'IJEN                          '&!          450
 ,'BRATAN                        '&!          451
 ,'BATUR                         '&!          452
 ,'AGUNG                         '&!          453
 ,'RINJANI                       '&!          454
 ,'TAMBORA                       '&!          455
 ,'SANGEANGAPI                   '&!          456
 ,'SANOWAI                       '&!          457
 ,'RANAKAH                       '&!          458
 ,'POCOLEOK                      '&!          459
 ,'INIERIE                       '&!          460
 ,'INIELIKA                      '&!          461
 ,'EBULOBO                       '&!          462
 ,'IYA                           '&!          463
 ,'SUKARIACALDERA                '&!          464
 ,'NDETENAPU                     '&!          465
 ,'KELIMUTU                      '&!          466
 ,'PALUWEH                       '&!          467
 ,'EGON                          '&!          468
 ,'ILIMUDA                       '&!          469
 ,'LEWOTOBI                      '&!          470
 ,'LEROBOLENG                    '&!          471
 ,'ILIBOLENG                     '&!          472
 ,'LEWOTOLO                      '&!          473
 ,'ILILABALEKAN                  '&!          474
 ,'ILIWERUNG                     '&!          475
 ,'TARABATU                      '&!          476
 ,'SIRUNG                        '&!          477
 ,'YERSEY                        '&!          478
 ,'EMPEROROFCHINA                '&!          479
 ,'NIEUWERKERK                   '&!          480
 ,'GUNUNGAPIWETAR                '&!          481
 ,'WURLALI                       '&!          482
 ,'TEON                          '&!          483
 ,'NILA                          '&!          484
 ,'SERUA                         '&!          485
 ,'MANUK                         '&!          486
 ,'BANDAAPI                      '&!          487
 ,'COLO[UNAUNA]                  '&!          488
 ,'AMBANG                        '&!          489
 ,'SOPUTAN                       '&!          490
 ,'SEMPU                         '&!          491
 ,'TONDANOCALDERA                '&!          492
 ,'LOKONEMPUNG                   '&!          493
 ,'MAHAWU                        '&!          494
 ,'KLABAT                        '&!          495
 ,'TONGKOKO                      '&!          496
 ,'RUANG                         '&!          497
 ,'KARANGETANG[APISIAU]          '&!          498
 ,'BANUAWUHU                     '&!          499
 ,'AWU                           '&!          500
 ,'UNNAMED                       '&!          501
 ,'TARAKAN                       '&!          502
 ,'DUKONO                        '&!          503
 ,'TOBARU                        '&!          504
 ,'IBU                           '&!          505
 ,'GAMKONORA                     '&!          506
 ,'JAILOLO                       '&!          507
 ,'HIRI                          '&!          508
 ,'TODOKORANU                    '&!          509
 ,'TIDORE                        '&!          510
 ,'MARE                          '&!          511
 ,'MOTI                          '&!          512
 ,'GAMALAMA                      '&!          513
 ,'TIGALALU                      '&!          514
 ,'AMASING                       '&!          515
 ,'BIBINOI                       '&!          516
 ,'MAKIAN                        '&!          517
 ,'BOMBALAI                      '&!          518
 ,'JOLO                          '&!          519
 ,'PARKER                        '&!          520
 ,'BALUT                         '&!          521
 ,'MATUTUM                       '&!          522
 ,'LEONARDRANGE                  '&!          523
 ,'APO                           '&!          524
 ,'MAKATURING                    '&!          525
 ,'LATUKAN                       '&!          526
 ,'KALATUNGAN                    '&!          527
 ,'RAGANG                        '&!          528
 ,'MALINDANG                     '&!          529
 ,'BALATUKAN                     '&!          530
 ,'MUSUAN                        '&!          531
 ,'CAMIGUIN                      '&!          532
 ,'PACO                          '&!          533
 ,'CUERNOSDENEGROS               '&!          534
 ,'KANLAON                       '&!          535
 ,'MANDALAGAN                    '&!          536
 ,'SILAY                         '&!          537
 ,'CABALIAN                      '&!          538
 ,'MAHAGNAO                      '&!          539
 ,'BILIRAN                       '&!          540
 ,'BULUSAN                       '&!          541
 ,'POCDOLMOUNTAINS               '&!          542
 ,'MASARAGA                      '&!          543
 ,'MAYON                         '&!          544
 ,'IRIGA                         '&!          545
 ,'ISAROG                        '&!          546
 ,'MALINDIG                      '&!          547
 ,'BANAHAW                       '&!          548
 ,'SANPABLOVOLCFIELD             '&!          549
 ,'TAAL                          '&!          550
 ,'MARIVELES                     '&!          551
 ,'NATIB                         '&!          552
 ,'PINATUBO                      '&!          553
 ,'ARAYAT                        '&!          554
 ,'AMORONG                       '&!          555
 ,'SANTOTOMAS                    '&!          556
 ,'PATOC                         '&!          557
 ,'AMBALATUNGANGROUP             '&!          558
 ,'LAGUNACALDERA                 '&!          559
 ,'CAGUA                         '&!          560
 ,'CAMIGUINDEBABUYANES           '&!          561
 ,'DIDICAS                       '&!          562
 ,'BABUYANCLARO                  '&!          563
 ,'UNNAMED                       '&!          564
 ,'IRAYA                         '&!          565
 ,'HAINANDAO                     '&!          566
 ,'LEIZHOUBANDAO                 '&!          567
 ,'CULAOREGROUP                  '&!          568
 ,'TOROENGPRONG                  '&!          569
 ,'HAUTDONGNAI                   '&!          570
 ,'BASDONGNAI                    '&!          571
 ,'CENDRESILEDES                 '&!          572
 ,'VETERAN                       '&!          573
 ,'POPA                          '&!          574
 ,'LOWERCHINDWIN                 '&!          575
 ,'SINGUPLATEAU                  '&!          576
 ,'TENGCHONG                     '&!          577
 ,'UNNAMED                       '&!          578
 ,'UNNAMED                       '&!          579
 ,'UNNAMED                       '&!          580
 ,'KUEISHANTAO                   '&!          581
 ,'UNNAMED                       '&!          582
 ,'UNNAMED                       '&!          583
 ,'ZENGYU                        '&!          584
 ,'IRIOMOTEJIMA                  '&!          585
 ,'YOKOATEJIMA                   '&!          586
 ,'AKUSEKIJIMA                   '&!          587
 ,'IWOTORISHIMA                  '&!          588
 ,'SUWANOSEJIMA                  '&!          589
 ,'KOGAJAJIMA                    '&!          590
 ,'KUCHINOSHIMA                  '&!          591
 ,'NAKANOSHIMA                   '&!          592
 ,'KUCHINOERABUJIMA              '&!          593
 ,'KIKAI                         '&!          594
 ,'IBUSUKIVOLCFIELD              '&!          595
 ,'SUMIYOSHIIKE                  '&!          596
 ,'SAKURAJIMA                    '&!          597
 ,'FUKUEJIMA                     '&!          598
 ,'KIRISHIMA                     '&!          599
 ,'UNZEN                         '&!          600
 ,'ASO                           '&!          601
 ,'KUJU                          '&!          602
 ,'TSURUMI                       '&!          603
 ,'ABU                           '&!          604
 ,'SANBE                         '&!          605
 ,'OKIDOGO                       '&!          606
 ,'IZUTOBU                       '&!          607
 ,'HAKONE                        '&!          608
 ,'KITAYATSUGATAKE               '&!          609
 ,'FUJI                          '&!          610
 ,'ONTAKE                        '&!          611
 ,'HAKUSAN                       '&!          612
 ,'NORIKURA                      '&!          613
 ,'WASHIBAKUMONOTAIRA            '&!          614
 ,'YAKEDAKE                      '&!          615
 ,'TATEYAMA                      '&!          616
 ,'NIIGATAYAKEYAMA               '&!          617
 ,'MYOKO                         '&!          618
 ,'ASAMA                         '&!          619
 ,'SHIGA                         '&!          620
 ,'HARUNA                        '&!          621
 ,'KUSATSUSHIRANE                '&!          622
 ,'HIUCHI                        '&!          623
 ,'AKAGI                         '&!          624
 ,'NANTAI                        '&!          625
 ,'OMANAGOGROUP                  '&!          626
 ,'TAKAHARA                      '&!          627
 ,'NIKKOSHIRANE                  '&!          628
 ,'NUMAZAWA                      '&!          629
 ,'NASU                          '&!          630
 ,'BANDAI                        '&!          631
 ,'ADATARA                       '&!          632
 ,'AZUMA                         '&!          633
 ,'HIJIORI                       '&!          634
 ,'ZAO                           '&!          635
 ,'NARUGO                        '&!          636
 ,'KURIKOMA                      '&!          637
 ,'CHOKAI                        '&!          638
 ,'AKITAKOMAGATAKE               '&!          639
 ,'IWATE                         '&!          640
 ,'HACHIMANTAI                   '&!          641
 ,'MEGATA                        '&!          642
 ,'AKITAYAKEYAMA                 '&!          643
 ,'TOWADA                        '&!          644
 ,'IWAKI                         '&!          645
 ,'HAKKODAGROUP                  '&!          646
 ,'OSOREYAMA                     '&!          647
 ,'TOSHIMA                       '&!          648
 ,'OSHIMA                        '&!          649
 ,'NIIJIMA                       '&!          650
 ,'KOZUSHIMA                     '&!          651
 ,'MIKURAJIMA                    '&!          652
 ,'KUROSEHOLE                    '&!          653
 ,'MIYAKEJIMA                    '&!          654
 ,'HACHIJOJIMA                   '&!          655
 ,'MYOJINKNOLL                   '&!          656
 ,'AOGASHIMA                     '&!          657
 ,'BAYONNAISEROCKS               '&!          658
 ,'SMITHROCK                     '&!          659
 ,'SOFUGAN                       '&!          660
 ,'SUIYOSEAMOUNT                 '&!          661
 ,'MOKUYOSEAMOUNT                '&!          662
 ,'DOYOSEAMOUNT                  '&!          663
 ,'NISHINOSHIMA                  '&!          664
 ,'KAIKATASEAMOUNT               '&!          665
 ,'TORISHIMA                     '&!          666
 ,'UNNAMED                       '&!          667
 ,'KAITOKUSEAMOUNT               '&!          668
 ,'KITAIWOJIMA                   '&!          669
 ,'KITAFUKUTOKUTAI               '&!          670
 ,'IOTO[IWOJIMA]                 '&!          671
 ,'MINAMIHIYOSHI                 '&!          672
 ,'NIKKO                         '&!          673
 ,'FUKUJIN                       '&!          674
 ,'KASUGA                        '&!          675
 ,'MINAMIKASUGA                  '&!          676
 ,'NWEIFUKU                      '&!          677
 ,'DAIKOKU                       '&!          678
 ,'UNNAMED                       '&!          679
 ,'UNNAMED                       '&!          680
 ,'FUKUTOKUOKANOBA               '&!          681
 ,'AHYI                          '&!          682
 ,'SUPPLYREEF                    '&!          683
 ,'MAUGISLANDS                   '&!          684
 ,'FARALLONDEPAJAROS             '&!          685
 ,'ASUNCION                      '&!          686
 ,'AGRIGAN                       '&!          687
 ,'PAGAN                         '&!          688
 ,'ALAMAGAN                      '&!          689
 ,'ZEALANDIABANK                 '&!          690
 ,'SARIGAN                       '&!          691
 ,'GUGUAN                        '&!          692
 ,'EASTDIAMANTE                  '&!          693
 ,'RUBY                          '&!          694
 ,'ANATAHAN                      '&!          695
 ,'NWROTA1                       '&!          696
 ,'ESMERALDABANK                 '&!          697
 ,'FORECASTSEAMOUNT              '&!          698
 ,'SEAMOUNTX                     '&!          699
 ,'ESAN                          '&!          700
 ,'OSHIMAOSHIMA                  '&!          701
 ,'KOMAGATAKE                    '&!          702
 ,'NISEKO                        '&!          703
 ,'YOTEI                         '&!          704
 ,'KUTTARA                       '&!          705
 ,'USU                           '&!          706
 ,'RISHIRI                       '&!          707
 ,'SHIKOTSU                      '&!          708
 ,'TOKACHI                       '&!          709
 ,'NIPESOTSUMARUYAMA             '&!          710
 ,'SHIKARIBETSUGROUP             '&!          711
 ,'DAISETSU                      '&!          712
 ,'AKAN                          '&!          713
 ,'MASHU                         '&!          714
 ,'RAUSU                         '&!          715
 ,'KUTCHARO                      '&!          716
 ,'SHIRETOKOIWOZAN               '&!          717
 ,'GOLOVNIN                      '&!          718
 ,'SMIRNOV                       '&!          719
 ,'MENDELEEV                     '&!          720
 ,'TIATIA                        '&!          721
 ,'LVINAYAPAST                   '&!          722
 ,'BERUTARUBE                    '&!          723
 ,'ATSONUPURI                    '&!          724
 ,'BOGATYRRIDGE                  '&!          725
 ,'UNNAMED                       '&!          726
 ,'GROZNYGROUP                   '&!          727
 ,'BARANSKY                      '&!          728
 ,'GOLETSTORNYIGROUP             '&!          729
 ,'CHIRIP                        '&!          730
 ,'MEDVEZHIA                     '&!          731
 ,'DEMON                         '&!          732
 ,'IVAOGROUP                     '&!          733
 ,'RUDAKOV                       '&!          734
 ,'TRISESTRY                     '&!          735
 ,'KOLOKOLGROUP                  '&!          736
 ,'UNNAMED                       '&!          737
 ,'CHIRPOI                       '&!          738
 ,'UNNAMED                       '&!          739
 ,'MILNE                         '&!          740
 ,'GORIASCHAIASOPKA              '&!          741
 ,'ZAVARITZKICALDERA             '&!          742
 ,'URATAMAN                      '&!          743
 ,'PREVOPEAK                     '&!          744
 ,'KETOI                         '&!          745
 ,'SREDNII                       '&!          746
 ,'USHISHUR                      '&!          747
 ,'RASSHUA                       '&!          748
 ,'UNNAMED                       '&!          749
 ,'SARYCHEVPEAK                  '&!          750
 ,'RAIKOKE                       '&!          751
 ,'CHIRINKOTAN                   '&!          752
 ,'EKARMA                        '&!          753
 ,'SINARKA                       '&!          754
 ,'KHARIMKOTAN                   '&!          755
 ,'TAORUSYRCALDERA               '&!          756
 ,'NEMOPEAK                      '&!          757
 ,'SHIRINKI                      '&!          758
 ,'FUSSPEAK                      '&!          759
 ,'LOMONOSOVGROUP                '&!          760
 ,'KARPINSKYGROUP                '&!          761
 ,'CHIKURACHKI                   '&!          762
 ,'VERNADSKIIRIDGE               '&!          763
 ,'EBEKO                         '&!          764
 ,'ALAID                         '&!          765
 ,'MASHKOVTSEV                   '&!          766
 ,'KAMBALNY                      '&!          767
 ,'YAVINSKY                      '&!          768
 ,'DIKYGREBEN                    '&!          769
 ,'KURILELAKE                    '&!          770
 ,'KOSHELEV                      '&!          771
 ,'ILYINSKY                      '&!          772
 ,'KELL                          '&!          773
 ,'BELENKAYA                     '&!          774
 ,'ZHELTOVSKY                    '&!          775
 ,'OZERNOY                       '&!          776
 ,'OLKOVIYVOLCGROUP              '&!          777
 ,'KHODUTKA                      '&!          778
 ,'PIRATKOVSKY                   '&!          779
 ,'OSTANETS                      '&!          780
 ,'OTDELNIY                      '&!          781
 ,'GOLAYA                        '&!          782
 ,'ASACHA                        '&!          783
 ,'VISOKIY                       '&!          784
 ,'KSUDACH                       '&!          785
 ,'MUTNOVSKY                     '&!          786
 ,'GORELY                        '&!          787
 ,'UNNAMED                       '&!          788
 ,'TOLMACHEVDOL                  '&!          789
 ,'VILYUCHIK                     '&!          790
 ,'BARKHATNAYASOPKA              '&!          791
 ,'UNNAMED                       '&!          792
 ,'UNNAMED                       '&!          793
 ,'BOLSHEBANNAYA                 '&!          794
 ,'OPALA                         '&!          795
 ,'KORYAKSKY                     '&!          796
 ,'AVACHINSKY                    '&!          797
 ,'DZENZURSKY                    '&!          798
 ,'VEER                          '&!          799
 ,'KOSTAKAN                      '&!          800
 ,'BAKENING                      '&!          801
 ,'ZAVARITSKY                    '&!          802
 ,'AKADEMIANAUK                  '&!          803
 ,'ZHUPANOVSKY                   '&!          804
 ,'KARYMSKY                      '&!          805
 ,'MALYSEMIACHIK                 '&!          806
 ,'BOLSHOISEMIACHIK              '&!          807
 ,'TAUNSHITS                     '&!          808
 ,'UZON                          '&!          809
 ,'KIKHPINYCH                    '&!          810
 ,'KRASHENINNIKOV                '&!          811
 ,'SCHMIDT                       '&!          812
 ,'KRONOTSKY                     '&!          813
 ,'GAMCHEN                       '&!          814
 ,'VYSOKY                        '&!          815
 ,'KOMAROV                       '&!          816
 ,'UNNAMED                       '&!          817
 ,'KIZIMEN                       '&!          818
 ,'UDINA                         '&!          819
 ,'ZIMINA                        '&!          820
 ,'TOLBACHIK                     '&!          821
 ,'KAMEN                         '&!          822
 ,'BEZYMIANNY                    '&!          823
 ,'USHKOVSKY                     '&!          824
 ,'KLIUCHEVSKOI                  '&!          825
 ,'PIIP                          '&!          826
 ,'KHANGAR                       '&!          827
 ,'CHERPUKGROUP                  '&!          828
 ,'SHIVELUCH                     '&!          829
 ,'ICHINSKY                      '&!          830
 ,'MALYPAYALPAN                  '&!          831
 ,'BOLSHOIPAYALPAN               '&!          832
 ,'PLOSKY                        '&!          833
 ,'AKHTANG                       '&!          834
 ,'KOZYREVSKY                    '&!          835
 ,'ROMANOVKA                     '&!          836
 ,'UKSICHAN                      '&!          837
 ,'BOLSHOIKEKUKNAYSKY            '&!          838
 ,'KULKEV                        '&!          839
 ,'GEODESISTOY                   '&!          840
 ,'ANAUN                         '&!          841
 ,'KRAINY                        '&!          842
 ,'KEKURNY                       '&!          843
 ,'EGGELLA                       '&!          844
 ,'UNNAMED                       '&!          845
 ,'VERKHOVOY                     '&!          846
 ,'ALNEYCHASHAKONDZHA            '&!          847
 ,'CHERNY                        '&!          848
 ,'POGRANYCHNY                   '&!          849
 ,'ZAOZERNY                      '&!          850
 ,'BLIZNETS                      '&!          851
 ,'KEBENEY                       '&!          852
 ,'FEDOTYCH                      '&!          853
 ,'SHISHEIKA                     '&!          854
 ,'TERPUK                        '&!          855
 ,'SEDANKINSKY                   '&!          856
 ,'LEUTONGEY                     '&!          857
 ,'TUZOVSKY                      '&!          858
 ,'GORNYINSTITUTE                '&!          859
 ,'KINENIN                       '&!          860
 ,'BLIZNETSY                     '&!          861
 ,'TITILA                        '&!          862
 ,'MEZHDUSOPOCHNY                '&!          863
 ,'SHISHEL                       '&!          864
 ,'ELOVSKY                       '&!          865
 ,'ALNGEY                        '&!          866
 ,'UKA                           '&!          867
 ,'KAILENEY                      '&!          868
 ,'PLOSKY                        '&!          869
 ,'BELY                          '&!          870
 ,'NYLGIMELKIN                   '&!          871
 ,'SNEZHNIY                      '&!          872
 ,'IKTUNUP                       '&!          873
 ,'SPOKOINY                      '&!          874
 ,'OSTRY                         '&!          875
 ,'SNEGOVOY                      '&!          876
 ,'SEVERNY                       '&!          877
 ,'IETTUNUP                      '&!          878
 ,'VOYAMPOLSKY                   '&!          879
 ,'SIKHOTEALIN                   '&!          880
 ,'UDOKANPLATEAU                 '&!          881
 ,'VITIMPLATEAU                  '&!          882
 ,'TUNKINDEPRESSION              '&!          883
 ,'OKAPLATEAU                    '&!          884
 ,'AZASPLATEAU                   '&!          885
 ,'TARYATUCHULUTU                '&!          886
 ,'KHANUYGOL                     '&!          887
 ,'BUSOBO                        '&!          888
 ,'DARIGANGAVOLCFIELD            '&!          889
 ,'MIDDLEGOBI                    '&!          890
 ,'TURFAN                        '&!          891
 ,'TIANSHANVOLCGROUP             '&!          892
 ,'KUNLUNVOLCGROUP               '&!          893
 ,'UNNAMED                       '&!          894
 ,'HONGGEERTU                    '&!          895
 ,'ARSHAN                        '&!          896
 ,'KELUOGROUP                    '&!          897
 ,'WUDALIANCHI                   '&!          898
 ,'JINGBO                        '&!          899
 ,'LONGGANGGROUP                 '&!          900
 ,'CHANGBAISHAN                  '&!          901
 ,'XIANJINDAO                    '&!          902
 ,'CHUGARYONG                    '&!          903
 ,'ULREUNG                       '&!          904
 ,'HALLA                         '&!          905
 ,'BULDIR                        '&!          906
 ,'KISKA                         '&!          907
 ,'SEGULA                        '&!          908
 ,'DAVIDOF                       '&!          909
 ,'LITTLESITKIN                  '&!          910
 ,'SEMISOPOCHNOI                 '&!          911
 ,'GARELOI                       '&!          912
 ,'TANAGA                        '&!          913
 ,'TAKAWANGHA                    '&!          914
 ,'BOBROF                        '&!          915
 ,'KANAGA                        '&!          916
 ,'MOFFETT                       '&!          917
 ,'GREATSITKIN                   '&!          918
 ,'KASATOCHI                     '&!          919
 ,'KONIUJI                       '&!          920
 ,'SERGIEF                       '&!          921
 ,'ATKA                          '&!          922
 ,'KOROVIN                       '&!          923
 ,'SEGUAM                        '&!          924
 ,'AMUKTA                        '&!          925
 ,'CHAGULAK                      '&!          926
 ,'YUNASKA                       '&!          927
 ,'HERBERT                       '&!          928
 ,'CARLISLE                      '&!          929
 ,'CLEVELAND                     '&!          930
 ,'TANA                          '&!          931
 ,'ULIAGA                        '&!          932
 ,'KAGAMIL                       '&!          933
 ,'VSEVIDOF                      '&!          934
 ,'RECHESCHNOI                   '&!          935
 ,'OKMOK                         '&!          936
 ,'BOGOSLOF                      '&!          937
 ,'MAKUSHIN                      '&!          938
 ,'AKUTAN                        '&!          939
 ,'WESTDAHL                      '&!          940
 ,'FISHER                        '&!          941
 ,'SHISHALDIN                    '&!          942
 ,'ISANOTSKI                     '&!          943
 ,'ROUNDTOP                      '&!          944
 ,'AMAK                          '&!          945
 ,'FROSTY                        '&!          946
 ,'DUTTON                        '&!          947
 ,'EMMONSLAKE                    '&!          948
 ,'PAVLOF                        '&!          949
 ,'PAVLOFSISTER                  '&!          950
 ,'DANA                          '&!          951
 ,'STEPOVAKBAY2                  '&!          952
 ,'STEPOVAKBAY3                  '&!          953
 ,'STEPOVAKBAY4                  '&!          954
 ,'KUPREANOF                     '&!          955
 ,'VENIAMINOF                    '&!          956
 ,'BLACKPEAK                     '&!          957
 ,'ANIAKCHAK                     '&!          958
 ,'YANTARNI                      '&!          959
 ,'CHIGINAGAK                    '&!          960
 ,'KIALAGVIK                     '&!          961
 ,'UGASHIKPEULIK                 '&!          962
 ,'UKINREKMAARS                  '&!          963
 ,'UNNAMED                       '&!          964
 ,'MARTIN                        '&!          965
 ,'MAGEIK                        '&!          966
 ,'TRIDENT                       '&!          967
 ,'KATMAI                        '&!          968
 ,'NOVARUPTA                     '&!          969
 ,'GRIGGS                        '&!          970
 ,'SNOWYMOUNTAIN                 '&!          971
 ,'DENISON                       '&!          972
 ,'STELLER                       '&!          973
 ,'KUKAK                         '&!          974
 ,'KAGUYAK                       '&!          975
 ,'FOURPEAKED                    '&!          976
 ,'DOUGLAS                       '&!          977
 ,'AUGUSTINE                     '&!          978
 ,'ILIAMNA                       '&!          979
 ,'REDOUBT                       '&!          980
 ,'SPURR                         '&!          981
 ,'HAYES                         '&!          982
 ,'STPAULISLAND                  '&!          983
 ,'NUNIVAKISLAND                 '&!          984
 ,'INGAKSLUGWATHILLS             '&!          985
 ,'ST.MICHAEL                    '&!          986
 ,'KOOKOOLIGITMOUNTAINS          '&!          987
 ,'IMURUKLAKE                    '&!          988
 ,'BUZZARDCREEK                  '&!          989
 ,'SANFORD                       '&!          990
 ,'WRANGELL                      '&!          991
 ,'GORDON                        '&!          992
 ,'CHURCHILL                     '&!          993
 ,'EDGECUMBE                     '&!          994
 ,'DUNCANCANAL                   '&!          995
 ,'TLEVAKSTRAITSUEMEZIS.         '&!          996
 ,'BEHMCANALRUDYERDBAY           '&!          997
 ,'FORTSELKIRK                   '&!          998
 ,'ALLIGATORLAKE                 '&!          999
 ,'ATLINVOLCFIELD                '&!         1000
 ,'TUYAVOLCFIELD                 '&!         1001
 ,'HEARTPEAKS                    '&!         1002
 ,'LEVELMOUNTAIN                 '&!         1003
 ,'EDZIZA                        '&!         1004
 ,'SPECTRUMRANGE                 '&!         1005
 ,'HOODOOMOUNTAIN                '&!         1006
 ,'ISKUTUNUKRIVERCONES           '&!         1007
 ,'TSEAXRIVERCONE                '&!         1008
 ,'CROWLAGOON                    '&!         1009
 ,'MILBANKESOUNDGROUP            '&!         1010
 ,'SATAHMOUNTAIN                 '&!         1011
 ,'NAZKO                         '&!         1012
 ,'WELLSGRAYCLEARWATER           '&!         1013
 ,'SILVERTHRONE                  '&!         1014
 ,'BRIDGERIVERCONES              '&!         1015
 ,'MEAGER                        '&!         1016
 ,'GARIBALDILAKE                 '&!         1017
 ,'GARIBALDI                     '&!         1018
 ,'BAKER                         '&!         1019
 ,'GLACIERPEAK                   '&!         1020
 ,'RAINIER                       '&!         1021
 ,'ADAMS                         '&!         1022
 ,'ST.HELENS                     '&!         1023
 ,'WESTCRATER                    '&!         1024
 ,'INDIANHEAVEN                  '&!         1025
 ,'HOOD                          '&!         1026
 ,'JEFFERSON                     '&!         1027
 ,'BLUELAKECRATER                '&!         1028
 ,'SANDMOUNTAINFIELD             '&!         1029
 ,'BELKNAP                       '&!         1030
 ,'NORTHSISTERFIELD              '&!         1031
 ,'SOUTHSISTER                   '&!         1032
 ,'BACHELOR                      '&!         1033
 ,'DAVISLAKE                     '&!         1034
 ,'NEWBERRY                      '&!         1035
 ,'DEVILSGARDEN                  '&!         1036
 ,'SQUAWRIDGELAVAFIELD           '&!         1037
 ,'FOURCRATERSLAVAFIELD          '&!         1038
 ,'CINNAMONBUTTE                 '&!         1039
 ,'CRATERLAKE                    '&!         1040
 ,'DIAMONDCRATERS                '&!         1041
 ,'JORDANCRATERS                 '&!         1042
 ,'SHASTA                        '&!         1043
 ,'MEDICINELAKE                  '&!         1044
 ,'BRUSHYBUTTE                   '&!         1045
 ,'TWINBUTTES                    '&!         1046
 ,'SILVERLAKE                    '&!         1047
 ,'TUMBLEBUTTES                  '&!         1048
 ,'LASSENVOLCCENTER              '&!         1049
 ,'EAGLELAKEFIELD                '&!         1050
 ,'CLEARLAKE                     '&!         1051
 ,'MONOLAKEVOLCFIELD             '&!         1052
 ,'MONOCRATERS                   '&!         1053
 ,'INYOCRATERS                   '&!         1054
 ,'MAMMOTHMOUNTAIN               '&!         1055
 ,'UBEHEBECRATERS                '&!         1056
 ,'GOLDENTROUTCREEK              '&!         1057
 ,'COSOVOLCFIELD                 '&!         1058
 ,'LAVICLAKE                     '&!         1059
 ,'SHOSHONELAVAFIELD             '&!         1060
 ,'CRATERSOFTHEMOON              '&!         1061
 ,'WAPILAVAFIELD                 '&!         1062
 ,'HELLSHALFACRE                 '&!         1063
 ,'YELLOWSTONE                   '&!         1064
 ,'SODALAKES                     '&!         1065
 ,'SANTACLARA                    '&!         1066
 ,'BALDKNOLL                     '&!         1067
 ,'MARKAGUNTPLATEAU              '&!         1068
 ,'BLACKROCKDESERT               '&!         1069
 ,'DOTSERO                       '&!         1070
 ,'UINKARETFIELD                 '&!         1071
 ,'SUNSETCRATER                  '&!         1072
 ,'CARRIZOZO                     '&!         1073
 ,'ZUNIBANDERA                   '&!         1074
 ,'ENDEAVOURRIDGE                '&!         1075
 ,'COBBSEGMENT                   '&!         1076
 ,'COAXIALSEGMENT                '&!         1077
 ,'AXIALSEAMOUNT                 '&!         1078
 ,'CLEFTSEGMENT                  '&!         1079
 ,'NORTHGORDARIDGE               '&!         1080
 ,'ESCANABASEGMENT               '&!         1081
 ,'UNNAMED                       '&!         1082
 ,'LOIHI                         '&!         1083
 ,'KILAUEA                       '&!         1084
 ,'MAUNALOA                      '&!         1085
 ,'MAUNAKEA                      '&!         1086
 ,'HUALALAI                      '&!         1087
 ,'HALEAKALA                     '&!         1088
 ,'UNNAMED                       '&!         1089
 ,'UNNAMED                       '&!         1090
 ,'TEAHITIA                      '&!         1091
 ,'ROCARD                        '&!         1092
 ,'MOUAPIHAA                     '&!         1093
 ,'MEHETIA                       '&!         1094
 ,'ADAMSSEAMOUNT                 '&!         1095
 ,'MACDONALD                     '&!         1096
 ,'NORTHERNEPRSEGMENTRO2         '&!         1097
 ,'NORTHERNEPRSEGMENTRO3         '&!         1098
 ,'UNNAMED                       '&!         1099
 ,'UNNAMED                       '&!         1100
 ,'GALAPAGOSRIFT                 '&!         1101
 ,'UNNAMED                       '&!         1102
 ,'SOUTHERNEPRSEGMENTK           '&!         1103
 ,'SOUTHERNEPRSEGMENTJ           '&!         1104
 ,'SOUTHERNEPRSEGMENTI           '&!         1105
 ,'ANTIPODESISLAND               '&!         1106
 ,'UNNAMED                       '&!         1107
 ,'UNNAMED                       '&!         1108
 ,'PRIETOCERRO                   '&!         1109
 ,'PINACATE                      '&!         1110
 ,'SANQUINTINVOLCFIELD           '&!         1111
 ,'SANLUISISLA                   '&!         1112
 ,'JARAGUAYVOLCFIELD             '&!         1113
 ,'CORONADO                      '&!         1114
 ,'GUADALUPE                     '&!         1115
 ,'SANBORJAVOLCFIELD             '&!         1116
 ,'UNNAMED                       '&!         1117
 ,'TORTUGAISLA                   '&!         1118
 ,'COMONDULAPURISIMA             '&!         1119
 ,'TRESVIRGENES                  '&!         1120
 ,'SOCORRO                       '&!         1121
 ,'DURANGOVOLCFIELD              '&!         1122
 ,'SANGANGUEY                    '&!         1123
 ,'BARCENA                       '&!         1124
 ,'MASCOTAVOLCFIELD              '&!         1125
 ,'CEBORUCO                      '&!         1126
 ,'COLIMA                        '&!         1127
 ,'ZITACUAROVALLEDEBRAVO         '&!         1128
 ,'JOCOTITLAN                    '&!         1129
 ,'MICHOACANGUANAJUATO           '&!         1130
 ,'TOLUCANEVADODE                '&!         1131
 ,'PAPAYO                        '&!         1132
 ,'IZTACCIHUATL                  '&!         1133
 ,'CHICHINAUTZIN                 '&!         1134
 ,'MALINCHELA                    '&!         1135
 ,'SERDANORIENTAL                '&!         1136
 ,'HUMEROSLOS                    '&!         1137
 ,'ATLIXCOSLOS                   '&!         1138
 ,'NAOLINCOVOLCFIELD             '&!         1139
 ,'COFREDEPEROTE                 '&!         1140
 ,'GLORIALA                      '&!         1141
 ,'CUMBRESLAS                    '&!         1142
 ,'POPOCATEPETL                  '&!         1143
 ,'ORIZABAPICODE                 '&!         1144
 ,'SANMARTIN                     '&!         1145
 ,'CHICHONEL                     '&!         1146
 ,'TACANA                        '&!         1147
 ,'TAJUMULCO                     '&!         1148
 ,'SANTAMARIA                    '&!         1149
 ,'ALMOLONGA                     '&!         1150
 ,'ATITLAN                       '&!         1151
 ,'TOLIMAN                       '&!         1152
 ,'ACATENANGO                    '&!         1153
 ,'FUEGO                         '&!         1154
 ,'AGUA                          '&!         1155
 ,'CUILAPABARBARENA              '&!         1156
 ,'PACAYA                        '&!         1157
 ,'JUMAYTEPEQUE                  '&!         1158
 ,'TECUAMBURRO                   '&!         1159
 ,'MOYUTA                        '&!         1160
 ,'FLORES                        '&!         1161
 ,'TAHUAL                        '&!         1162
 ,'SANTIAGOCERRO                 '&!         1163
 ,'SUCHITAN                      '&!         1164
 ,'CHINGO                        '&!         1165
 ,'IXTEPEQUE                     '&!         1166
 ,'IPALA                         '&!         1167
 ,'CHIQUIMULAVOLCFIELD           '&!         1168
 ,'QUEZALTEPEQUE                 '&!         1169
 ,'SANDIEGO                      '&!         1170
 ,'SINGUILCERRO                  '&!         1171
 ,'APANECARANGE                  '&!         1172
 ,'SANTAANA                      '&!         1173
 ,'IZALCO                        '&!         1174
 ,'COATEPEQUECALDERA             '&!         1175
 ,'CINOTEPEQUECERRO              '&!         1176
 ,'GUAZAPA                       '&!         1177
 ,'SANSALVADOR                   '&!         1178
 ,'ILOPANGO                      '&!         1179
 ,'APASTEPEQUEFIELD              '&!         1180
 ,'TABURETE                      '&!         1181
 ,'SANVICENTE                    '&!         1182
 ,'USULUTAN                      '&!         1183
 ,'TIGREEL                       '&!         1184
 ,'TECAPA                        '&!         1185
 ,'CHINAMECA                     '&!         1186
 ,'ARAMUACALAGUNA                '&!         1187
 ,'SANMIGUEL                     '&!         1188
 ,'CONCHAGUA                     '&!         1189
 ,'CONCHAGUITA                   '&!         1190
 ,'TIGREISLAEL                   '&!         1191
 ,'ZACATEGRANDEISLA              '&!         1192
 ,'YOJOALAKE                     '&!         1193
 ,'UTILAISLAND                   '&!         1194
 ,'COSIGUINA                     '&!         1195
 ,'SANCRISTOBAL                  '&!         1196
 ,'TELICA                        '&!         1197
 ,'ROTA                          '&!         1198
 ,'NEGROCERRO                    '&!         1199
 ,'PILASLAS                      '&!         1200
 ,'APOYEQUE                      '&!         1201
 ,'NEJAPAMIRAFLORES              '&!         1202
 ,'MOMOTOMBO                     '&!         1203
 ,'GRANADA                       '&!         1204
 ,'MASAYA                        '&!         1205
 ,'ZAPATERA                      '&!         1206
 ,'MOMBACHO                      '&!         1207
 ,'CONCEPCION                    '&!         1208
 ,'MADERAS                       '&!         1209
 ,'ESTELI                        '&!         1210
 ,'CIGUATEPECERROEL              '&!         1211
 ,'LAJASLAS                      '&!         1212
 ,'AZULVOLCAN                    '&!         1213
 ,'OROSI                         '&!         1214
 ,'RINCONDELAVIEJA               '&!         1215
 ,'TENORIO                       '&!         1216
 ,'ARENAL                        '&!         1217
 ,'PLATANAR                      '&!         1218
 ,'MIRAVALLES                    '&!         1219
 ,'POAS                          '&!         1220
 ,'BARVA                         '&!         1221
 ,'IRAZU                         '&!         1222
 ,'TURRIALBA                     '&!         1223
 ,'BARU                          '&!         1224
 ,'YEGUADALA                     '&!         1225
 ,'VALLEEL                       '&!         1226
 ,'ROMERAL                       '&!         1227
 ,'BRAVOCERRO                    '&!         1228
 ,'SANTAISABEL                   '&!         1229
 ,'RUIZNEVADODEL                 '&!         1230
 ,'TOLIMANEVADODEL               '&!         1231
 ,'MACHIN                        '&!         1232
 ,'HUILANEVADODEL                '&!         1233
 ,'SOTARA                        '&!         1234
 ,'PETACAS                       '&!         1235
 ,'PURACE                        '&!         1236
 ,'DOAJUANA                      '&!         1237
 ,'GALERAS                       '&!         1238
 ,'AZUFRAL                       '&!         1239
 ,'CUMBAL                        '&!         1240
 ,'NEGRODEMAYASQUERCERRO         '&!         1241
 ,'SOCHE                         '&!         1242
 ,'CHACHIMBIRO                   '&!         1243
 ,'CUICOCHA                      '&!         1244
 ,'IMBABURA                      '&!         1245
 ,'MOJANDA                       '&!         1246
 ,'CAYAMBE                       '&!         1247
 ,'PULULAGUA                     '&!         1248
 ,'REVENTADOR                    '&!         1249
 ,'ATACAZO                       '&!         1250
 ,'CHACANA                       '&!         1251
 ,'GUAGUAPICHINCHA               '&!         1252
 ,'ANTISANA                      '&!         1253
 ,'ILLINIZA                      '&!         1254
 ,'SUMACO                        '&!         1255
 ,'COTOPAXI                      '&!         1256
 ,'QUILOTOA                      '&!         1257
 ,'CHIMBORAZO                    '&!         1258
 ,'LICTO                         '&!         1259
 ,'TUNGURAHUA                    '&!         1260
 ,'SANGAY                        '&!         1261
 ,'ECUADOR                       '&!         1262
 ,'FERNANDINA                    '&!         1263
 ,'WOLF                          '&!         1264
 ,'DARWIN                        '&!         1265
 ,'ALCEDO                        '&!         1266
 ,'NEGRASIERRA                   '&!         1267
 ,'AZULCERRO                     '&!         1268
 ,'PINTA                         '&!         1269
 ,'GENOVESA                      '&!         1270
 ,'MARCHENA                      '&!         1271
 ,'SANTACRUZ                     '&!         1272
 ,'SANTIAGO                      '&!         1273
 ,'SANCRISTOBAL                  '&!         1274
 ,'QUIMSACHATA                   '&!         1275
 ,'AUQUIHUATOCERRO               '&!         1276
 ,'SARASARA                      '&!         1277
 ,'COROPUNA                      '&!         1278
 ,'ANDAHUAORCOPAMPA              '&!         1279
 ,'HUAMBO                        '&!         1280
 ,'SABANCAYA                     '&!         1281
 ,'CHACHANINEVADO                '&!         1282
 ,'NICHOLSONCERRO                '&!         1283
 ,'MISTIEL                       '&!         1284
 ,'UBINAS                        '&!         1285
 ,'TICSANI                       '&!         1286
 ,'HUAYNAPUTINA                  '&!         1287
 ,'TUTUPACA                      '&!         1288
 ,'YUCAMANE                      '&!         1289
 ,'CASIRINEVADOS                 '&!         1290
 ,'TAAPACA                       '&!         1291
 ,'PARINACOTA                    '&!         1292
 ,'TACORA                        '&!         1293
 ,'TAMBOQUEMADO                  '&!         1294
 ,'GUALLATIRI                    '&!         1295
 ,'TATASABAYA                    '&!         1296
 ,'JAYUKHOTALAGUNA               '&!         1297
 ,'NUEVOMUNDO                    '&!         1298
 ,'ISLUGA                        '&!         1299
 ,'PAMPALUXSAR                   '&!         1300
 ,'IRRUPUTUNCU                   '&!         1301
 ,'OLCAPARUMA                    '&!         1302
 ,'AZUFRECERRODEL                '&!         1303
 ,'OLLAGUE                       '&!         1304
 ,'SANPEDRO                      '&!         1305
 ,'SAIRECABUR                    '&!         1306
 ,'LICANCABUR                    '&!         1307
 ,'GUAYAQUES                     '&!         1308
 ,'PURICOCOMPLEX                 '&!         1309
 ,'COLACHI                       '&!         1310
 ,'ACAMARACHI                    '&!         1311
 ,'OVEROCERRO                    '&!         1312
 ,'CHILIQUES                     '&!         1313
 ,'PUTANA                        '&!         1314
 ,'CORDONDEPUNTASNEGRAS          '&!         1315
 ,'MIIQUES                       '&!         1316
 ,'TUJLECERRO                    '&!         1317
 ,'CAICHINQUE                    '&!         1318
 ,'TILOCALAR                     '&!         1319
 ,'NEGRILLAREL                   '&!         1320
 ,'PULAR                         '&!         1321
 ,'NEGRILLARLA                   '&!         1322
 ,'SOCOMPA                       '&!         1323
 ,'LASCAR                        '&!         1324
 ,'ESCORIALCERRO                 '&!         1325
 ,'LLULLAILLACO                  '&!         1326
 ,'CORDONDELAZUFRE               '&!         1327
 ,'BAYOCERRO                     '&!         1328
 ,'NEVADASIERRA                  '&!         1329
 ,'FALSOAZUFRE                   '&!         1330
 ,'INCAHUASINEVADODE             '&!         1331
 ,'LASTARRIA                     '&!         1332
 ,'SOLOEL                        '&!         1333
 ,'OJOSDELSALADONEVADOS          '&!         1334
 ,'COPIAPO                       '&!         1335
 ,'TUZGLECERRO                   '&!         1336
 ,'ARACAR                        '&!         1337
 ,'UNNAMED                       '&!         1338
 ,'ANTOFAGASTADELASIERRA         '&!         1339
 ,'CONDORCERROEL                 '&!         1340
 ,'PEINADO                       '&!         1341
 ,'ROBLEDO                       '&!         1342
 ,'TIPAS                         '&!         1343
 ,'EASTERISLAND                  '&!         1344
 ,'SANFELIX                      '&!         1345
 ,'ROBINSONCRUSOE                '&!         1346
 ,'UNNAMED                       '&!         1347
 ,'TUPUNGATITO                   '&!         1348
 ,'MAIPO                         '&!         1349
 ,'PALOMO                        '&!         1350
 ,'ATUELCALDERADEL               '&!         1351
 ,'RISCOPLATEADO                 '&!         1352
 ,'SANJOSE                       '&!         1353
 ,'TINGUIRIRICA                  '&!         1354
 ,'CALABOZOS                     '&!         1355
 ,'PLANCHONPETEROA               '&!         1356
 ,'DESCABEZADOGRANDE             '&!         1357
 ,'MAULELAGUNADEL                '&!         1358
 ,'SANPEDROPELLADO               '&!         1359
 ,'LONGAVINEVADODE               '&!         1360
 ,'BLANCASLOMAS                  '&!         1361
 ,'RESAGO                        '&!         1362
 ,'PAYUNMATRU                    '&!         1363
 ,'DOMUYO                        '&!         1364
 ,'AZULCERRO                     '&!         1365
 ,'COCHIQUITOVOLCGROUP           '&!         1366
 ,'TROMEN                        '&!         1367
 ,'PUESTOCORTADERAS              '&!         1368
 ,'CHILLANNEVADOSDE              '&!         1369
 ,'TROCON                        '&!         1370
 ,'ANTUCO                        '&!         1371
 ,'CALLAQUI                      '&!         1372
 ,'MARIAQUILAGUNA                '&!         1373
 ,'TOLGUACA                      '&!         1374
 ,'COPAHUE                       '&!         1375
 ,'LONQUIMAY                     '&!         1376
 ,'SOLLIPULLI                    '&!         1377
 ,'CABURGUAHUELEMOLLE            '&!         1378
 ,'LLAIMA                        '&!         1379
 ,'QUETRUPILLAN                  '&!         1380
 ,'LANIN                         '&!         1381
 ,'HUANQUIHUEGROUP               '&!         1382
 ,'VILLARRICA                    '&!         1383
 ,'MOCHOCHOSHUENCO               '&!         1384
 ,'CARRANLOSVENADOS              '&!         1385
 ,'PANTOJACERRO                  '&!         1386
 ,'ANTILLANCAGROUP               '&!         1387
 ,'PUYEHUECORDONCAULLE           '&!         1388
 ,'PUNTIAGUDOCORDONCENIZOS       '&!         1389
 ,'TRONADOR                      '&!         1390
 ,'CAYUTUELAVIGUERIA             '&!         1391
 ,'OSORNO                        '&!         1392
 ,'CUERNOSDELDIABLO              '&!         1393
 ,'YATE                          '&!         1394
 ,'HORNOPIREN                    '&!         1395
 ,'APAGADO                       '&!         1396
 ,'CRATERBASALTVOLCFIELD         '&!         1397
 ,'CALBUCO                       '&!         1398
 ,'HUEQUI                        '&!         1399
 ,'CHAITEN                       '&!         1400
 ,'MINCHINMAVIDA                 '&!         1401
 ,'YANTELES                      '&!         1402
 ,'PALENAVOLCGROUP               '&!         1403
 ,'MELIMOYU                      '&!         1404
 ,'PUYUHUAPI                     '&!         1405
 ,'MENTOLAT                      '&!         1406
 ,'CAY                           '&!         1407
 ,'MACA                          '&!         1408
 ,'HUDSONCERRO                   '&!         1409
 ,'RIOMURTA                      '&!         1410
 ,'ARENALES                      '&!         1411
 ,'CORCOVADO                     '&!         1412
 ,'VIEDMA                        '&!         1413
 ,'AGUILERA                      '&!         1414
 ,'RECLUS                        '&!         1415
 ,'LAUTARO                       '&!         1416
 ,'BURNEYMONTE                   '&!         1417
 ,'PALEIAIKEVOLCFIELD            '&!         1418
 ,'FUEGUINO                      '&!         1419
 ,'SABA                          '&!         1420
 ,'QUILLTHE                      '&!         1421
 ,'LIAMUIGA                      '&!         1422
 ,'NEVISPEAK                     '&!         1423
 ,'SOUFRIEREHILLS                '&!         1424
 ,'SOUFRIEREGUADELOUPE           '&!         1425
 ,'DIABLESMORNEAUX               '&!         1426
 ,'DIABLOTINSMORNE               '&!         1427
 ,'WATTMORNE                     '&!         1428
 ,'TROISPITONSMORNE              '&!         1429
 ,'PLATPAYSMORNE                 '&!         1430
 ,'PELEE                         '&!         1431
 ,'QUALIBOU                      '&!         1432
 ,'SOUFRIEREST.VINCENT           '&!         1433
 ,'KICKEMJENNY                   '&!         1434
 ,'STCATHERINE                   '&!         1435
 ,'SNAEFELLSJOKULL               '&!         1436
 ,'HELGRINDUR                    '&!         1437
 ,'LJOSUFJOLL                    '&!         1438
 ,'REYKJANES                     '&!         1439
 ,'KRISUVIK                      '&!         1440
 ,'BRENNISTEINSFJOLL             '&!         1441
 ,'HROMUNDARTINDUR               '&!         1442
 ,'HENGILL                       '&!         1443
 ,'GRIMSNES                      '&!         1444
 ,'PRESTAHNUKUR                  '&!         1445
 ,'HVERAVELLIR                   '&!         1446
 ,'HOFSJOKULL                    '&!         1447
 ,'VESTMANNAEYJAR                '&!         1448
 ,'EYJAFJOLL                     '&!         1449
 ,'KATLA                         '&!         1450
 ,'TINDFJALLAJOKULL              '&!         1451
 ,'TORFAJOKULL                   '&!         1452
 ,'HEKLA                         '&!         1453
 ,'GRIMSVOTN                     '&!         1454
 ,'BARDARBUNGA                   '&!         1455
 ,'TUNGNAFELLSJOKULL             '&!         1456
 ,'KVERKFJOLL                    '&!         1457
 ,'ASKJA                         '&!         1458
 ,'FREMRINAMUR                   '&!         1459
 ,'KRAFLA                        '&!         1460
 ,'THEISTAREYKJARBUNGA           '&!         1461
 ,'TJORNESFRACTUREZONE           '&!         1462
 ,'ORAEFAJOKULL                  '&!         1463
 ,'ESJUFJOLL                     '&!         1464
 ,'KOLBEINSEYRIDGE               '&!         1465
 ,'JANMAYEN                      '&!         1466
 ,'UNNAMED                       '&!         1467
 ,'UNNAMED                       '&!         1468
 ,'UNNAMED                       '&!         1469
 ,'UNNAMED                       '&!         1470
 ,'UNNAMED                       '&!         1471
 ,'FLORES                        '&!         1472
 ,'CORVO                         '&!         1473
 ,'FAYAL                         '&!         1474
 ,'PICO                          '&!         1475
 ,'SANJORGE                      '&!         1476
 ,'GRACIOSA                      '&!         1477
 ,'TERCEIRA                      '&!         1478
 ,'DONJOAODECASTROBANK           '&!         1479
 ,'PICOSVOLCSYSTEM               '&!         1480
 ,'SETECIDADES                   '&!         1481
 ,'AGUADEPAU                     '&!         1482
 ,'FURNAS                        '&!         1483
 ,'MONACOBANK                    '&!         1484
 ,'MADEIRA                       '&!         1485
 ,'LAPALMA                       '&!         1486
 ,'HIERRO                        '&!         1487
 ,'TENERIFE                      '&!         1488
 ,'GRANCANARIA                   '&!         1489
 ,'FUERTEVENTURA                 '&!         1490
 ,'LANZAROTE                     '&!         1491
 ,'FOGO                          '&!         1492
 ,'BRAVA                         '&!         1493
 ,'SAOVICENTE                    '&!         1494
 ,'UNNAMED                       '&!         1495
 ,'UNNAMED                       '&!         1496
 ,'UNNAMED                       '&!         1497
 ,'UNNAMED                       '&!         1498
 ,'ASCENSION                     '&!         1499
 ,'TRINDADE                      '&!         1500
 ,'NIGHTINGALEISLAND             '&!         1501
 ,'TRISTANDACUNHA                '&!         1502
 ,'BOUVET                        '&!         1503
 ,'THOMPSONISLAND                '&!         1504
 ,'YOUNGISLAND                   '&!         1505
 ,'STURGEISLAND                  '&!         1506
 ,'PLEIADESTHE                   '&!         1507
 ,'UNNAMED                       '&!         1508
 ,'MELBOURNE                     '&!         1509
 ,'UNNAMED                       '&!         1510
 ,'BUCKLEISLAND                  '&!         1511
 ,'ROYALSOCIETYRANGE             '&!         1512
 ,'BERLIN                        '&!         1513     
 ,'ANDRUS                        '&!         1514
 ,'WAESCHE                       '&!         1515
 ,'SIPLE                         '&!         1516
 ,'TONEYMOUNTAIN                 '&!         1517
 ,'TAKAHE                        '&!         1518
 ,'HUDSONMOUNTAINS               '&!         1519
 ,'PETERIISLAND                  '&!         1520
 ,'EREBUS                        '&!         1521
 ,'PENGUINISLAND                 '&!         1522
 ,'DECEPTIONISLAND               '&!         1523
 ,'PAULET                        '&!         1524
 ,'BRIDGEMANISLAND               '&!         1525
 ,'SEALNUNATAKSGROUP             '&!         1526
 ,'THULEISLANDS                  '&!         1527
 ,'MONTAGUISLAND                 '&!         1528
 ,'BRISTOLISLAND                 '&!         1529
 ,'MICHAEL                       '&!         1530
 ,'CANDLEMASISLAND               '&!         1531
 ,'HODSON                        '&!         1532
 ,'LESKOVISLAND                  '&!         1533
 ,'ZAVODOVSKI                    '&!         1534
 ,'PROTECTORSHOAL                '&!         1535
/)

!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
real,parameter,    dimension(6,nvolcanoes) :: volcanoes_data=RESHAPE((/&
!data volcanoes/  &
!---------------------------------------------------------------------------------------------------
! Volcanoes
!---------------------------------------------------------------------------------------------------
!    Lat     | Long |  Elevation !Time | Eruption | Turn on/off |name  
!            |      |    (m)	 |Frame| Type     |  emission 	|
!---------------------------------------------------------------------------------------------------
    50.1700,    6.8500,    600.00,  7.,  5., 1., &! ,'WESTEIFELVOLCFIELD	    '&! 	   1	  
    45.7750,    2.9700,   1464.00,  7.,  1., 1., &! ,'CHAINEDESPUYS		    '&! 	   2	  
    42.1700,    2.5300,    893.00, -1.,  1., 1., &! ,'OLOTVOLCFIELD		    '&! 	   3	  
    38.8700,   -4.0200,   1117.00,  7.,  1., 1., &! ,'CALATRAVAVOLCFIELD	    '&! 	   4	  
    43.2500,   10.8700,    500.00,  6.,  5., 1., &! ,'LARDERELLO		    '&! 	   5	  
    42.6000,   11.9300,    800.00,  7.,  5., 1., &! ,'VULSINI			    '&! 	   6	  
    41.7300,   12.7000,    949.00, -2.,  5., 1., &! ,'ALBANHILLS		    '&! 	   7	  
    40.8270,   14.1390,    458.00,  5.,  5., 1., &! ,'CAMPIFLEGREI		    '&! 	   8	  
    40.8210,   14.4260,   1281.00,  2.,  7., 1., &! ,'VESUVIUS  		    '&! 	   9	  
    40.7300,   13.8970,    789.00,  6.,  5., 1., &! ,'ISCHIA			    '&! 	  10	  
    38.6300,   15.0700,    421.00, -2.,  5., 1., &! ,'PANAREA			    '&! 	  11	  
    38.4800,   14.9500,    602.00,  6.,  5., 1., &! ,'LIPARI			    '&! 	  12	  
    38.7890,   15.2130,    924.00,  1.,  2., 1., &! ,'STROMBOLI 		    '&! 	  13	  
    38.4040,   14.9620,    500.00,  3.,  7., 1., &! ,'VULCANO			    '&! 	  14	  
    37.7340,   15.0040,   3330.00,  1.,  2., 1., &! ,'ETNA			    '&! 	  15	  
    36.7700,   12.0200,    836.00,  3.,  5., 1., &! ,'PANTELLERIA		    '&! 	  16	  
    37.1000,   12.7000,     -8.00,  3.,  1., 1., &! ,'CAMPIFLEGREIMARSICILIA	    '&! 	  17	  
    37.6150,   23.3360,    760.00,  7.,  5., 1., &! ,'METHANA			    '&! 	  18	  
    36.6990,   24.4390,    751.00,  6.,  5., 1., &! ,'MILOS			    '&! 	  19	  
    36.4040,   25.3960,    367.00,  2.,  6., 1., &! ,'SANTORINI 		    '&! 	  20	  
    36.6710,   27.1400,    180.00, -1.,  5., 1., &! ,'YALI			    '&! 	  21	  
    36.5860,   27.1600,    698.00,  3.,  5., 1., &! ,'NISYROS			    '&! 	  22	  
    38.5800,   28.5200,    750.00, -1.,  1., 1., &! ,'KULA			    '&! 	  23	  
    37.6700,   33.6500,   1302.00, -1.,  1., 1., &! ,'KARAPINARFIELD		    '&! 	  24	  
    38.1300,   34.1700,   3253.00,  7.,  5., 1., &! ,'HASANDAGI 		    '&! 	  25	  
    38.2500,   34.5700,   2143.00, -2.,  5., 1., &! ,'GOLLUDAG  		    '&! 	  26	  
    38.5700,   34.5200,   1689.00,  7.,  5., 1., &! ,'ACIGOLNEVSEHIR		    '&! 	  27	  
    37.6700,   39.8300,   1957.00, -1.,  1., 1., &! ,'KARACADAG 		    '&! 	  28	  
    38.5200,   35.4800,   3916.00, -2.,  5., 1., &! ,'ERCIYESDAGI		    '&! 	  29	  
    38.9200,   42.8200,   4158.00,  7.,  5., 1., &! ,'SUPHANDAGI		    '&! 	  30	  
    39.1700,   43.3300,      0.00, -1.,  5., 1., &! ,'GIREKOL			    '&! 	  31	  
    38.6500,   42.2300,   2948.00,  5.,  5., 1., &! ,'NEMRUTDAGI		    '&! 	  32	  
    39.3700,   43.8700,   3584.00,  3.,  1., 1., &! ,'TENDUREKDAGI		    '&! 	  33	  
    39.7000,   44.3000,   5165.00,  3.,  5., 1., &! ,'ARARAT			    '&! 	  34	  
    40.7500,   42.9000,   3000.00, -2.,  5., 1., &! ,'KARSPLATEAU		    '&! 	  35	  
    43.3300,   42.4500,   5633.00,  6.,  5., 1., &! ,'ELBRUS			    '&! 	  36	  
    42.7000,   44.5000,   5050.00,  7.,  5., 1., &! ,'KASBEK			    '&! 	  37	  
    42.5500,   44.0000,   3650.00, -1.,  5., 1., &! ,'KABARGINOTHGROUP  	    '&! 	  38	  
    42.4500,   44.2500,   3750.00, -1.,  5., 1., &! ,'UNNAMED			    '&! 	  39	  
    41.5500,   43.6000,   3400.00, -1.,  5., 1., &! ,'UNNAMED			    '&! 	  40	  
    40.5300,   44.2000,   4095.00, -1.,  5., 1., &! ,'ARAGATS			    '&! 	  41	  
    40.2750,   44.7500,   3597.00,  7.,  5., 1., &! ,'GHEGAMRIDGE		    '&! 	  42	  
    39.7000,   45.5420,   3329.00,  7.,  5., 1., &! ,'DARALAGES 		    '&! 	  43	  
    40.0200,   45.7800,   2800.00,  7.,  5., 1., &! ,'PORAK			    '&! 	  44	  
    39.7300,   46.0200,   3000.00,  7.,  5., 1., &! ,'TSKHOUKKARCKAR		    '&! 	  45	  
    15.5500,   41.8300,    244.00,  1.,  2., 1., &! ,'TAIRJEBELAT		    '&! 	  46	  
    14.0200,   42.7500,    624.00, -1.,  1., 1., &! ,'ZUKUR			    '&! 	  47	  
    13.7200,   42.7300,    422.00, -1.,  1., 1., &! ,'HANISH			    '&! 	  48	  
    15.0500,   42.1800,    191.00,  3.,  1., 1., &! ,'ZUBAIRJEBEL		    '&! 	  49	  
    15.0420,   39.8200,    713.00, -1.,  1., 1., &! ,'JALUA			    '&! 	  50	  
    14.2420,   40.3000,    -48.00,  2.,  5., 1., &! ,'DALLOL			    '&! 	  51	  
    14.8800,   39.9200,    904.00, -1.,  5., 1., &! ,'ALID			    '&! 	  52	  
    13.9750,   40.4080,    287.00, -1.,  1., 1., &! ,'GADAALE			    '&! 	  53	  
    13.8250,   40.5080,    429.00, -1.,  1., 1., &! ,'ALU			    '&! 	  54	  
    13.7250,   40.6000,    668.00, -1.,  1., 1., &! ,'BORALEALE 		    '&! 	  55	  
    13.7920,   40.5500,    613.00,  1.,  1., 1., &! ,'DALAFFILLA		    '&! 	  56	  
    13.6000,   40.6700,    613.00,  1.,  2., 1., &! ,'ERTAALE			    '&! 	  57	  
    13.5000,   40.7200,    521.00, -1.,  1., 1., &! ,'HAYLIGUBBI		    '&! 	  58	  
    13.5200,   40.6300,   1031.00, -1.,  1., 1., &! ,'ALEBAGU			    '&! 	  59	  
    13.3700,   41.7000,   2218.00, -2.,  5., 1., &! ,'NABRO			    '&! 	  60	  
    13.2700,   41.6500,   1875.00, -2.,  5., 1., &! ,'MALLAHLE  		    '&! 	  61	  
    13.1800,   41.7250,   1611.00, -2.,  1., 1., &! ,'SORKALE			    '&! 	  62	  
    13.0700,   41.6000,   1200.00, -1.,  1., 1., &! ,'ASAVYO			    '&! 	  63	  
    13.1000,   41.1500,    523.00, -1.,  1., 1., &! ,'MATALA			    '&! 	  64	  
    13.2800,   41.0700,    700.00, -1.,  1., 1., &! ,'TATALI			    '&! 	  65	  
    13.3000,   40.9800,    812.00, -1.,  1., 1., &! ,'BORAWLI			    '&! 	  66	  
    13.5800,   41.8080,   1625.00,  3.,  1., 1., &! ,'DUBBI			    '&! 	  67	  
    13.0200,   40.2000,   1815.00, -1.,  5., 1., &! ,'MAALALTA  		    '&! 	  68	  
    12.8800,   40.5700,   1501.00,  2.,  1., 1., &! ,'ALAYTA			    '&! 	  69	  
    12.6000,   40.4800,   1442.00,  1.,  5., 1., &! ,'DABBAHU			    '&! 	  70	  
    12.3800,   40.0700,   1302.00, -1.,  1., 1., &! ,'DABBAYRA  		    '&! 	  71	  
    12.1700,   40.8200,    600.00,  1.,  1., 1., &! ,'MANDAHARARO		    '&! 	  72	  
    11.7300,   40.2500,    930.00, -1.,  5., 1., &! ,'GROPPO			    '&! 	  73	  
    13.0800,   40.8500,   1295.00, -2.,  5., 1., &! ,'AFDERA			    '&! 	  74	  
    11.6300,   41.4500,    875.00, -1.,  5., 1., &! ,'BORAWLI			    '&! 	  75	  
    12.3800,   42.2000,    600.00,  2.,  1., 1., &! ,'MANDAINAKIR		    '&! 	  76	  
    12.4700,   42.4000,   2028.00, -1.,  5., 1., &! ,'MOUSAALLI 		    '&! 	  77	  
    12.5500,   42.5300,    600.00, -1.,  1., 1., &! ,'GUFA			    '&! 	  78	  
    12.9500,   42.4300,    987.00, -1.,  1., 1., &! ,'ASSABVOLCFIELD		    '&! 	  79	  
    11.5800,   42.4700,    298.00,  2.,  1., 1., &! ,'ARDOUKOBA 		    '&! 	  80	  
    11.8800,   41.2080,    625.00, -1.,  1., 1., &! ,'KURUB			    '&! 	  81	  
    11.2800,   41.6300,   1068.00,  5.,  1., 1., &! ,'DAMAALI			    '&! 	  82	  
    10.5800,   41.0420,   1383.00, -1.,  5., 1., &! ,'YANGUDI			    '&! 	  83	  
    11.0800,   41.2700,   1459.00, -1.,  5., 1., &! ,'GABILLEMA 		    '&! 	  84	  
    10.0820,   40.7020,   2145.00, -1.,  5., 1., &! ,'AYELU			    '&! 	  85	  
     9.7800,   40.3300,    900.00, -1.,  1., 1., &! ,'HERTALI			    '&! 	  86	  
     9.5700,   40.2800,    878.00, -2.,  5., 1., &! ,'LIADOHAYK 		    '&! 	  87	  
    10.0700,   40.8400,   1733.00, -1.,  5., 1., &! ,'ADWA			    '&! 	  88	  
     9.3500,   40.1300,   1151.00, -1.,  5., 1., &! ,'DOFEN			    '&! 	  89	  
     8.9500,   39.7500,   1100.00, -1.,  1., 1., &! ,'BERU			    '&! 	  90	  
     8.9750,   39.9300,   2007.00,  3.,  5., 1., &! ,'FENTALE			    '&! 	  91	  
     8.8000,   39.6920,   1619.00,  3.,  5., 1., &! ,'KONE			    '&! 	  92	  
     8.7000,   39.6300,   1300.00, -1.,  1., 1., &! ,'UNNAMED			    '&! 	  93	  
     8.5580,   39.4750,   2447.00, -1.,  5., 1., &! ,'BOSETBERICHA		    '&! 	  94	  
     8.7800,   38.9800,   1850.00, -1.,  1., 1., &! ,'BISHOFTUVOLCFIELD 	    '&! 	  95	  
     8.6200,   38.9500,   1800.00, -1.,  1., 1., &! ,'UNNAMED			    '&! 	  96	  
     8.4300,   39.3500,   1765.00, -1.,  1., 1., &! ,'SODORE			    '&! 	  97	  
     8.3500,   39.1800,   1984.00, -1.,  5., 1., &! ,'GEDAMSACALDERA		    '&! 	  98	  
     8.2700,   39.0300,   2285.00, -1.,  5., 1., &! ,'BORABERICCIO		    '&! 	  99	  
     8.1580,   39.1300,   2349.00,  2.,  5., 1., &! ,'TULLUMOJE 		    '&! 	 100	  
     8.0700,   39.0700,   1800.00, -1.,  1., 1., &! ,'UNNAMED			    '&! 	 101	  
     7.9500,   38.9300,   1889.00, -1.,  1., 1., &! ,'EASTZWAY  		    '&! 	 102	  
     8.0500,   38.3500,   2281.00, -1.,  1., 1., &! ,'BUTAJIRISILTIFIELD	    '&! 	 103	  
     7.7700,   38.7800,   2335.00,  7.,  5., 1., &! ,'ALUTU			    '&! 	 104	  
     7.4700,   38.5800,   2075.00, -1.,  5., 1., &! ,'OACALDERA 		    '&! 	 105	  
     7.1800,   38.4300,   2320.00, -1.,  5., 1., &! ,'CORBETTICALDERA		    '&! 	 106	  
     7.0700,   38.1000,   1700.00, -1.,  5., 1., &! ,'BILATERIVERFIELD  	    '&! 	 107	  
     7.4200,   35.4300,   2728.00, -1.,  1., 1., &! ,'TEPI			    '&! 	 108	  
     6.7800,   37.8300,   1800.00, -2.,  1., 1., &! ,'HOBICHACALDERA		    '&! 	 109	  
     6.6500,   38.1200,   1650.00, -2.,  5., 1., &! ,'CHIRACHA  		    '&! 	 110	  
     5.9300,   37.5700,   1650.00, -1.,  1., 1., &! ,'TOSASUCHA 		    '&! 	 111	  
     5.6500,   37.6700,   1200.00, -1.,  1., 1., &! ,'UNNAMED			    '&! 	 112	  
     5.1000,   35.8800,    912.00, -2.,  1., 1., &! ,'KORATHRANGE		    '&! 	 113	  
     4.0800,   37.4200,   1067.00, -1.,  1., 1., &! ,'MEGABASALTFIELD		    '&! 	 114	  
     4.0700,   36.0500,    520.00, -1.,  5., 1., &! ,'NORTHISLAND		    '&! 	 115	  
     3.5000,   36.0420,    550.00, -1.,  1., 1., &! ,'CENTRALISLAND		    '&! 	 116	  
     2.3200,   37.9700,   1707.00, -2.,  1., 1., &! ,'MARSABIT  		    '&! 	 117	  
     2.6300,   36.6000,    800.00,  3.,  1., 1., &! ,'SOUTHISLAND		    '&! 	 118	  
     2.3200,   36.5700,   1032.00,  2.,  6., 1., &! ,'BARRIERTHE		    '&! 	 119	  
     1.9800,   36.4300,    817.00,  7.,  1., 1., &! ,'NAMARUNU  		    '&! 	 120	  
     1.5700,   37.9000,    699.00, -1.,  1., 1., &! ,'SEGERERUAPLATEAU  	    '&! 	 121	  
     1.5000,   36.3300,   1328.00,  2.,  5., 1., &! ,'EMURUANGOGOLAK		    '&! 	 122	  
     1.1500,   36.2300,   1528.00,  7.,  5., 1., &! ,'SILALI			    '&! 	 123	  
     0.9200,   36.1800,   1697.00,  7.,  5., 1., &! ,'PAKA			    '&! 	 124	  
     0.7700,   36.1200,   1446.00, -1.,  5., 1., &! ,'KOROSI			    '&! 	 125	  
     0.6200,   36.0750,   1130.00, -1.,  1., 1., &! ,'OLKOKWE			    '&! 	 126	  
     0.2300,   37.8700,    750.00, -1.,  1., 1., &! ,'NYAMBENIHILLS		    '&! 	 127	  
    -0.2000,   36.0700,   2278.00,  7.,  5., 1., &! ,'MENENGAI  		    '&! 	 128	  
    -0.5200,   36.2700,   2126.00, -1.,  1., 1., &! ,'ELMENTEITABADLANDS	    '&! 	 129	  
    -0.3800,   34.5000,   1751.00, -1.,  1., 1., &! ,'HOMAMOUNTAIN		    '&! 	 130	  
    -0.6500,   36.2200,   2856.00, -1.,  5., 1., &! ,'EBURRUOLDOINYO		    '&! 	 131	  
    -0.9040,   36.2920,   2434.00,  4.,  5., 1., &! ,'OLKARIA			    '&! 	 132	  
    -0.9140,   36.4460,   2776.00,  3.,  5., 1., &! ,'LONGONOT  		    '&! 	 133	  
    -1.1750,   36.3500,   2356.00, -1.,  5., 1., &! ,'SUSWA			    '&! 	 134	  
    -2.7640,   35.9140,   2962.00,  1.,  7., 1., &! ,'LENGAIOLDOINYO		    '&! 	 135	  
    -2.6800,   37.8800,   2188.00,  3.,  1., 1., &! ,'CHYULUHILLS		    '&! 	 136	  
    -3.0700,   37.3500,   5895.00, -1.,  5., 1., &! ,'KILIMANJARO		    '&! 	 137	  
    -4.8700,   31.9200,      0.00, -1.,  5., 1., &! ,'IGWISIHILLS		    '&! 	 138	  
    -8.6300,   33.5700,      0.00, -1.,  1., 1., &! ,'UNNAMED			    '&! 	 139	  
    -8.7500,   33.8000,   2179.00, -1.,  5., 1., &! ,'SWUSANGUBASIN		    '&! 	 140	  
    -8.9700,   33.5700,   2622.00, -1.,  5., 1., &! ,'NGOZI			    '&! 	 141	  
    -8.9300,   33.4000,   1568.00, -1.,  5., 1., &! ,'IZUMBWEMPOLI		    '&! 	 142	  
    -9.1300,   33.6700,   2961.00, -1.,  5., 1., &! ,'RUNGWE			    '&! 	 143	  
    -3.2500,   36.7500,   4565.00,  2.,  5., 1., &! ,'MERU			    '&! 	 144	  
    -9.2300,   33.7800,   2175.00,  3.,  5., 1., &! ,'KIEYO			    '&! 	 145	  
     0.7000,   30.2500,   1615.00,  7.,  1., 1., &! ,'FORTPORTAL		    '&! 	 146	  
     0.4500,   30.2500,   1430.00, -2.,  5., 1., &! ,'KYATWA			    '&! 	 147	  
    -0.0800,   29.9200,   1067.00, -1.,  5., 1., &! ,'KATWEKIKORONGO		    '&! 	 148	  
    -0.2000,   30.0800,   1554.00, -1.,  5., 1., &! ,'BUNYARUGURU		    '&! 	 149	  
    -0.4710,   30.1910,   1707.00, -1.,  5., 1., &! ,'KATUNGA			    '&! 	 150	  
    -0.9300,   29.3300,    950.00, -2.,  5., 1., &! ,'MAYYAMOTO 		    '&! 	 151	  
    -1.4080,   29.2000,   3058.00,  1.,  2., 1., &! ,'NYAMURAGIRA		    '&! 	 152	  
    -1.5200,   29.2500,   3470.00,  1.,  2., 1., &! ,'NYIRAGONGO		    '&! 	 153	  
    -1.5000,   29.4500,   4507.00,  7.,  1., 1., &! ,'KARISIMBI 		    '&! 	 154	  
    -1.4700,   29.4920,   3711.00,  2.,  5., 1., &! ,'VISOKE			    '&! 	 155	  
    -1.3800,   29.6700,   4127.00, -1.,  1., 1., &! ,'MUHAVURA  		    '&! 	 156	  
    -1.2300,   29.7200,   2440.00, -2.,  1., 1., &! ,'BUFUMBIRA 		    '&! 	 157	  
    -2.3200,   28.7500,   1460.00, -1.,  1., 1., &! ,'TSHIBINDA 		    '&! 	 158	  
     0.2000,    6.5800,   2024.00, -2.,  1., 1., &! ,'SAOTOME			    '&! 	 159	  
     3.3500,    8.5200,   2260.00, -1.,  1., 1., &! ,'SANCARLOS 		    '&! 	 160	  
     3.3500,    8.6300,   2009.00, -1.,  1., 1., &! ,'SANJOAQUIN		    '&! 	 161	  
     3.5800,    8.7500,   3007.00,  2.,  1., 1., &! ,'SANTAISABEL		    '&! 	 162	  
     4.7500,    9.6700,    500.00, -1.,  1., 1., &! ,'TOMBELGRABEN		    '&! 	 163	  
     4.2030,    9.1700,   4095.00,  1.,  2., 1., &! ,'CAMEROON  		    '&! 	 164	  
     5.0300,    9.8300,   2411.00, -2.,  1., 1., &! ,'MANENGOUBA		    '&! 	 165	  
     6.2500,   10.5000,   3011.00, -2.,  1., 1., &! ,'OKUVOLCFIELD		    '&! 	 166	  
     7.2500,   13.6700,      0.00, -2.,  1., 1., &! ,'NGAOUNDEREPLATEAU 	    '&! 	 167	  
    10.7500,   12.0000,      0.00, -2.,  1., 1., &! ,'BIUPLATEAU		    '&! 	 168	  
    17.6800,    8.5000,   1780.00, -1.,  1., 1., &! ,'TODRAVOLCFIELD		    '&! 	 169	  
    19.8300,    2.8300,      0.00, -1.,  1., 1., &! ,'TINZAOUATENEVOLCFIELD	    '&! 	 170	  
    23.0000,   10.8300,      0.00, -2.,  1., 1., &! ,'INEZZANEVOLCFIELD 	    '&! 	 171	  
    22.6700,    5.0000,   1467.00, -1.,  1., 1., &! ,'TAHALRAVOLCFIELD  	    '&! 	 172	  
    23.3300,    5.8300,   2918.00, -1.,  1., 1., &! ,'ATAKORVOLCFIELD		    '&! 	 173	  
    23.9200,    5.8300,   1672.00, -1.,  1., 1., &! ,'MANZAZVOLCFIELD		    '&! 	 174	  
    27.2500,   17.5000,   1200.00, -1.,  1., 1., &! ,'HARUJ			    '&! 	 175	  
    25.0500,   17.5500,    547.00, -2.,  1., 1., &! ,'WAUENNAMUS		    '&! 	 176	  
    21.3300,   16.3300,   2000.00, -1.,  1., 1., &! ,'TOHTARSO  		    '&! 	 177	  
    21.0300,   16.4500,   3265.00, -1.,  5., 1., &! ,'TOUSSIDETARSO		    '&! 	 178	  
    19.8000,   18.5300,   3415.00, -1.,  5., 1., &! ,'KOUSSIEMI 		    '&! 	 179	  
    20.9200,   17.2800,   3100.00, -1.,  5., 1., &! ,'VOONTARSO 		    '&! 	 180	  
    12.9500,   24.2700,   3042.00,  7.,  1., 1., &! ,'MARRAJEBEL		    '&! 	 181	  
    14.5700,   25.8500,      0.00, -2.,  1., 1., &! ,'KUTUMVOLCFIELD		    '&! 	 182	  
    15.3200,   26.4700,   2000.00,  7.,  1., 1., &! ,'MEIDOBVOLCFIELD		    '&! 	 183	  
    18.3300,   32.7500,    670.00,  6.,  1., 1., &! ,'BAYUDAVOLCFIELD		    '&! 	 184	  
    18.1700,   33.8300,      0.00, -2.,  1., 1., &! ,'UMMARAFIEBJEBEL		    '&! 	 185	  
    36.5300,   40.8500,    534.00, -1.,  1., 1., &! ,'SHARATKOVAKAB		    '&! 	 186	  
    36.6700,   37.0000,      0.00,  6.,  1., 1., &! ,'UNNAMED			    '&! 	 187	  
    33.1000,   35.9700,   1197.00, -1.,  1., 1., &! ,'GOLANHEIGHTS		    '&! 	 188	  
    33.0000,   36.4300,    945.00, -1.,  1., 1., &! ,'UNNAMED			    '&! 	 189	  
    33.2500,   37.0700,    979.00,  3.,  1., 1., &! ,'ESSAFA			    '&! 	 190	  
    32.6580,   36.4250,   1803.00, -1.,  1., 1., &! ,'DRUZEJABALAD		    '&! 	 191	  
    31.0800,   38.4200,   1100.00, -1.,  1., 1., &! ,'HARRAHAL  		    '&! 	 192	  
    27.8000,   36.1700,   1950.00, -1.,  1., 1., &! ,'RAHAHHARRATAR		    '&! 	 193	  
    27.0800,   37.2500,   1920.00,  6.,  1., 1., &! ,'UWAYRIDHARRAT		    '&! 	 194	  
    25.1700,   37.7500,   1370.00,  6.,  1., 1., &! ,'LUNAYYIRHARRAT		    '&! 	 195	  
    26.5800,   40.2000,   1625.00, -1.,  1., 1., &! ,'ITHNAYNHARRAT		    '&! 	 196	  
    25.0000,   39.9200,   2093.00,  6.,  1., 1., &! ,'KHAYBARHARRAT		    '&! 	 197	  
    22.8000,   41.3800,   1475.00, -1.,  1., 1., &! ,'KISHBHARRAT		    '&! 	 198	  
    18.3700,   41.6300,    381.00, -1.,  1., 1., &! ,'BIRKHARRATAL		    '&! 	 199	  
    23.0800,   39.7800,   1744.00,  6.,  1., 1., &! ,'RAHATHARRAT		    '&! 	 200	  
    17.0500,   42.8300,    305.00,  3.,  1., 1., &! ,'YARJABAL  		    '&! 	 201	  
    15.6300,   44.0800,   3100.00,  6.,  1., 1., &! ,'ARHABHARRAOF		    '&! 	 202	  
    15.2450,   44.2360,   2506.00, -2.,  1., 1., &! ,'MARHAJABALEL		    '&! 	 203	  
    15.4300,   44.7800,   1550.00,  7.,  1., 1., &! ,'HAYLANJABAL		    '&! 	 204	  
    14.5700,   44.6700,   3500.00,  2.,  1., 1., &! ,'DHAMARHARRASOF		    '&! 	 205	  
    12.2500,   45.0000,      0.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	 206	  
    13.5800,   46.1200,   1737.00,  6.,  1., 1., &! ,'SAWADHARRAES		    '&! 	 207	  
    14.0500,   48.3300,    233.00, -1.,  1., 1., &! ,'BALHAFHARRAOF		    '&! 	 208	  
    15.5500,   50.6300,      0.00, -2.,  1., 1., &! ,'BIRBORHUT 		    '&! 	 209	  
    39.3300,   45.1700,      0.00, -1.,  5., 1., &! ,'UNNAMED			    '&! 	 210	  
    37.7500,   46.4300,   3707.00, -1.,  5., 1., &! ,'SAHAND			    '&! 	 211	  
    38.2500,   47.9200,   4811.00, -1.,  5., 1., &! ,'SABALAN			    '&! 	 212	  
    35.9510,   52.1090,   5670.00,  7.,  5., 1., &! ,'DAMAVAND  		    '&! 	 213	  
    29.4000,   57.5700,      0.00, -2.,  1., 1., &! ,'QALEHHASANALI		    '&! 	 214	  
    28.0700,   60.0000,   3490.00, -1.,  5., 1., &! ,'BAZMAN			    '&! 	 215	  
    28.1700,   60.6700,      0.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	 216	  
    28.6000,   61.1300,   3940.00, -1.,  5., 1., &! ,'TAFTAN			    '&! 	 217	  
    33.9500,   67.9200,   3800.00, -2.,  5., 1., &! ,'DACHTINAVARGROUP  	    '&! 	 218	  
    34.2500,   67.9700,   3190.00, -2.,  5., 1., &! ,'VAKAKGROUP		    '&! 	 219	  
   -11.4700,   43.3300,   1087.00, -1.,  1., 1., &! ,'GRILLELA  		    '&! 	 220	  
   -12.6000,   49.1500,   1475.00, -1.,  1., 1., &! ,'AMBREBOBAOMBY		    '&! 	 221	  
   -13.3200,   48.4800,    214.00, -1.,  5., 1., &! ,'NOSYBE			    '&! 	 222	  
   -14.3000,   48.6700,   2878.00, -1.,  1., 1., &! ,'ANKAIZINAFIELD		    '&! 	 223	  
   -19.0000,   46.7700,   1800.00,  7.,  5., 1., &! ,'ITASYVOLCFIELD		    '&! 	 224	  
   -19.4000,   47.2000,   2644.00, -1.,  5., 1., &! ,'ANKARATRAFIELD		    '&! 	 225	  
   -11.7500,   43.3800,   2361.00,  1.,  2., 1., &! ,'KARTHALA  		    '&! 	 226	  
   -21.2310,   55.7130,   2632.00,  1.,  2., 1., &! ,'FOURNAISEPITONDELA	    '&! 	 227	  
   -37.7210,   77.8250,   -650.00,  2.,  2., 1., &! ,'BOOMERANGSEAMOUNT 	    '&! 	 228	  
   -37.8300,   77.5200,    881.00, -1.,  1., 1., &! ,'AMSTERDAMISLAND		    '&! 	 229	  
   -38.7200,   77.5300,    268.00,  4.,  1., 1., &! ,'ST.PAUL			    '&! 	 230	  
   -53.0300,   72.6000,    230.00,  1.,  6., 1., &! ,'MCDONALDISLANDS		    '&! 	 231	  
   -53.1060,   73.5130,   2745.00,  1.,  2., 1., &! ,'HEARD			    '&! 	 232	  
   -49.5800,   69.5000,   1840.00, -2.,  5., 1., &! ,'KERGUELENISLANDS  	    '&! 	 233	  
   -46.4300,   52.2000,   1090.00, -2.,  1., 1., &! ,'ESTILEDEL 		    '&! 	 234	  
   -46.4200,   51.7500,    934.00, -1.,  1., 1., &! ,'POSSESSIONILEDELA 	    '&! 	 235	  
   -46.1000,   50.2300,    775.00, -1.,  1., 1., &! ,'COCHONSILEAUX		    '&! 	 236	  
   -46.6300,   37.9500,    672.00, -1.,  1., 1., &! ,'PRINCEEDWARDISLAND	    '&! 	 237	  
   -46.9000,   37.7500,   1230.00,  1.,  2., 1., &! ,'MARIONISLAND		    '&! 	 238	  
    11.7500,   80.7500,      0.00, -2.,  2., 1., &! ,'UNNAMED			    '&! 	 239	  
   -35.7500,  174.2700,    397.00, -2.,  1., 1., &! ,'WHANGAREI 		    '&! 	 240	  
   -35.3000,  173.9000,    388.00,  6.,  1., 1., &! ,'KAIKOHEBAYOFISLANDS	    '&! 	 241	  
   -37.2800,  176.2500,    355.00,  7.,  5., 1., &! ,'MAYORISLAND		    '&! 	 242	  
   -36.9000,  174.8700,    260.00,  6.,  1., 1., &! ,'AUCKLANDFIELD		    '&! 	 243	  
   -39.3000,  174.0700,   2518.00,  3.,  5., 1., &! ,'TARANAKI[EGMONT]  	    '&! 	 244	  
   -37.5200,  177.1800,    321.00,  1.,  6., 1., &! ,'WHITEISLAND		    '&! 	 245	  
   -38.1200,  176.5000,   1111.00,  2.,  5., 1., &! ,'OKATAINA  		    '&! 	 246	  
   -38.4200,  176.3300,    592.00,  6.,  5., 1., &! ,'REPOROA			    '&! 	 247	  
   -38.4200,  176.0800,   1156.00,  6.,  5., 1., &! ,'MAROA			    '&! 	 248	  
   -38.8200,  176.0000,    760.00,  6.,  5., 1., &! ,'TAUPO			    '&! 	 249	  
   -39.1300,  175.6420,   1978.00,  2.,  6., 1., &! ,'TONGARIRO 		    '&! 	 250	  
   -36.4460,  177.8390,   -860.00, -1.,  6., 1., &! ,'CLARK			    '&! 	 251	  
   -36.3210,  178.0280,    600.00, -1.,  6., 1., &! ,'TANGAROA  		    '&! 	 252	  
   -39.2800,  175.5700,   2797.00,  1.,  6., 1., &! ,'RUAPEHU			    '&! 	 253	  
   -36.1420,  178.1960,    400.00, -1.,  6., 1., &! ,'RUMBLEV			    '&! 	 254	  
   -36.1300,  178.0500,    500.00, -1.,  6., 1., &! ,'RUMBLEIV  		    '&! 	 255	  
   -35.7450,  178.4780,   -220.00,  2.,  6., 1., &! ,'RUMBLEIII 		    '&! 	 256	  
   -35.3530,  178.5270,   1200.00, -1.,  6., 1., &! ,'RUMBLEIIWEST		    '&! 	 257	  
   -35.0040,  178.9730,    980.00,  6.,  6., 1., &! ,'HEALY			    '&! 	 258	  
   -34.8750,  179.0750,  -1350.00, -1.,  6., 1., &! ,'BROTHERS  		    '&! 	 259	  
   -31.8500, -179.1800,   -900.00, -1.,  6., 1., &! ,'VOLCANOW  		    '&! 	 260	  
   -30.5420, -178.5610,    137.00, -2.,  5., 1., &! ,'CURTISISLAND		    '&! 	 261	  
   -30.2000, -178.4700,    238.00,  7.,  1., 1., &! ,'MACAULEYISLAND		    '&! 	 262	  
   -30.0360, -178.7120,    -65.00, -1.,  1., 1., &! ,'GIGGENBACH		    '&! 	 263	  
   -29.2700, -177.9200,    516.00,  1.,  5., 1., &! ,'RAOULISLAND		    '&! 	 264	  
   -25.8870, -177.1880,   -132.00,  1.,  5., 1., &! ,'MONOWAISEAMOUNT		    '&! 	 265	  
   -24.8000, -177.0200,   -385.00, -1.,  5., 1., &! ,'UNNAMED			    '&! 	 266	  
   -21.1500, -175.7500,    -65.00, -1.,  5., 1., &! ,'UNNAMED			    '&! 	 267	  
   -21.3800, -175.6500,   -500.00,  2.,  5., 1., &! ,'UNNAMED			    '&! 	 268	  
   -20.8500, -175.5300,    -13.00,  2.,  5., 1., &! ,'UNNAMED			    '&! 	 269	  
   -20.5700, -175.3800,    149.00,  2.,  5., 1., &! ,'HUNGATONGAHUNGAHAAPAI	    '&! 	 270	  
   -20.3200, -175.4200,    -17.00,  2.,  5., 1., &! ,'FALCONISLAND		    '&! 	 271	  
   -19.6700, -175.0300,   1030.00, -1.,  5., 1., &! ,'KAO			    '&! 	 272	  
   -19.7500, -175.0700,    515.00,  2.,  5., 1., &! ,'TOFUA			    '&! 	 273	  
   -19.1800, -174.8700,     43.00,  2.,  5., 1., &! ,'METISSHOAL		    '&! 	 274	  
   -18.9920, -174.7750,     -2.00,  1.,  5., 1., &! ,'HOMEREEF  		    '&! 	 275	  
   -18.3250, -174.3650,    -40.00,  1.,  5., 1., &! ,'UNNAMED			    '&! 	 276	  
   -18.8060, -174.6500,    540.00,  3.,  5., 1., &! ,'LATE			    '&! 	 277	  
   -15.8500, -173.7200,    560.00, -2.,  5., 1., &! ,'TAFAHI			    '&! 	 278	  
   -15.6200, -173.6700,    -33.00,  2.,  5., 1., &! ,'CURACOA			    '&! 	 279	  
   -18.0200, -174.3250,    180.00,  2.,  5., 1., &! ,'FONUALEI  		    '&! 	 280	  
   -15.6000, -175.6300,    260.00,  2.,  1., 1., &! ,'NIUAFOOU  		    '&! 	 281	  
   -14.2150, -169.0580,   -592.00,  1.,  1., 1., &! ,'VAILULUU  		    '&! 	 282	  
   -14.2300, -169.4540,    931.00, -1.,  1., 1., &! ,'TAU			    '&! 	 283	  
   -14.1750, -169.6180,    639.00,  3.,  1., 1., &! ,'OFUOLOSEGA		    '&! 	 284	  
   -14.2950, -170.7000,    653.00, -1.,  1., 1., &! ,'TUTUILA			    '&! 	 285	  
   -13.9350, -171.7200,   1100.00, -1.,  1., 1., &! ,'UPOLU			    '&! 	 286	  
   -13.6120, -172.5250,   1858.00,  2.,  1., 1., &! ,'SAVAII			    '&! 	 287	  
   -13.3000, -176.1700,    143.00, -1.,  1., 1., &! ,'WALLISISLANDS		    '&! 	 288	  
   -16.8200, -179.9700,   1241.00,  5.,  1., 1., &! ,'TAVEUNI			    '&! 	 289	  
   -17.3200,  179.4000,    522.00, -2.,  1., 1., &! ,'KORO			    '&! 	 290	  
   -19.1200,  177.9800,    805.00,  5.,  5., 1., &! ,'NABUKELEVU		    '&! 	 291	  
    -2.3800,  147.3500,    270.00,  2.,  5., 1., &! ,'STANDREWSTRAIT		    '&! 	 292	  
    -2.5700,  147.2800,    254.00, -1.,  1., 1., &! ,'BALUAN			    '&! 	 293	  
    -3.0300,  147.7800,  -1300.00,  2.,  1., 1., &! ,'UNNAMED			    '&! 	 294	  
    -3.5070,  144.6050,    402.00, -1.,  5., 1., &! ,'BLUPBLUP  		    '&! 	 295	  
    -3.6300,  144.6310,    365.00, -1.,  5., 1., &! ,'KADOVAR			    '&! 	 296	  
    -3.9940,  144.9630,    240.00, -1.,  1., 1., &! ,'BOISA			    '&! 	 297	  
    -3.6130,  144.8180,    685.00,  2.,  7., 1., &! ,'BAM			    '&! 	 298	  
    -4.0800,  145.0370,   1807.00,  1.,  2., 1., &! ,'MANAM			    '&! 	 299	  
    -4.6490,  145.9640,   1839.00,  2.,  6., 1., &! ,'KARKAR			    '&! 	 300	  
    -4.9000,  146.7500,      0.00, -2.,  6., 1., &! ,'YOMBA			    '&! 	 301	  
    -4.3110,  146.2560,  -2000.00, -2.,  6., 1., &! ,'UNNAMED			    '&! 	 302	  
    -5.3580,  147.1200,   1280.00,  2.,  6., 1., &! ,'LONGISLAND		    '&! 	 303	  
    -5.5890,  147.8750,   1548.00, -1.,  1., 1., &! ,'UMBOI			    '&! 	 304	  
    -5.5200,  148.1210,    140.00,  1.,  2., 1., &! ,'RITTERISLAND		    '&! 	 305	  
    -5.4140,  148.0940,    992.00, -2.,  1., 1., &! ,'SAKAR			    '&! 	 306	  
    -5.2000,  148.5700,      0.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	 307	  
    -5.5250,  148.4200,   1330.00,  1.,  2., 1., &! ,'LANGILA			    '&! 	 308	  
    -4.6300,  149.3500,    179.00, -1.,  1., 1., &! ,'MUNDUA			    '&! 	 309	  
    -4.6920,  149.5000,    368.00, -1.,  5., 1., &! ,'GAROVE			    '&! 	 310	  
    -5.0560,  150.1080,    400.00,  3.,  5., 1., &! ,'DAKATAUA  		    '&! 	 311	  
    -5.1500,  150.0300,   1155.00, -1.,  5., 1., &! ,'BOLA			    '&! 	 312	  
    -5.3000,  150.0700,    565.00, -2.,  5., 1., &! ,'GARUAHARBOUR		    '&! 	 313	  
    -5.4680,  150.5070,    805.00, -2.,  5., 1., &! ,'LOLO			    '&! 	 314	  
    -5.4500,  150.0300,    564.00,  1.,  5., 1., &! ,'GARBUNAGROUP		    '&! 	 315	  
    -5.5800,  150.5200,    742.00,  1.,  6., 1., &! ,'PAGO			    '&! 	 316	  
    -5.5000,  150.9420,    610.00, -1.,  5., 1., &! ,'SULURANGE 		    '&! 	 317	  
    -5.3300,  151.1000,   1148.00,  6.,  5., 1., &! ,'HARGY			    '&! 	 318	  
    -5.2000,  151.2300,   2248.00,  3.,  5., 1., &! ,'BAMUS			    '&! 	 319	  
    -5.0500,  151.3300,   2334.00,  1.,  7., 1., &! ,'ULAWUN			    '&! 	 320	  
    -4.7500,  150.8500,      0.00, -2.,  7., 1., &! ,'UNNAMED			    '&! 	 321	  
    -4.9200,  151.1580,    858.00,  2.,  4., 1., &! ,'LOLOBAU			    '&! 	 322	  
    -4.2710,  152.2030,    688.00,  1.,  6., 1., &! ,'RABAUL			    '&! 	 323	  
    -4.1200,  152.2000,    200.00,  7.,  5., 1., &! ,'TAVUI			    '&! 	 324	  
    -5.9000,  143.1500,   3568.00, -2.,  5., 1., &! ,'DOMAPEAKS 		    '&! 	 325	  
    -6.5800,  145.0800,   3233.00, -2.,  1., 1., &! ,'CRATERMOUNTAIN		    '&! 	 326	  
    -7.0500,  145.8580,   3384.00, -2.,  5., 1., &! ,'YELIA			    '&! 	 327	  
    -7.3300,  146.7080,   1500.00, -1.,  5., 1., &! ,'KORANGA			    '&! 	 328	  
    -9.2000,  147.5700,    850.00, -1.,  1., 1., &! ,'MADILOGO  		    '&! 	 329	  
    -9.0000,  148.3700,   1915.00, -1.,  5., 1., &! ,'HYDROGRAPHERSRANGE	    '&! 	 330	  
    -8.9500,  148.1500,   1680.00,  2.,  7., 1., &! ,'LAMINGTON 		    '&! 	 331	  
    -9.0800,  148.3300,   1342.00, -1.,  1., 1., &! ,'MANAGLASEPLATEAU  	    '&! 	 332	  
    -9.3080,  148.1300,    808.00, -2.,  5., 1., &! ,'MUSARIVER 		    '&! 	 333	  
    -9.4800,  149.1300,    370.00, -1.,  5., 1., &! ,'SESSAGARA 		    '&! 	 334	  
    -9.2000,  149.0700,   1925.00,  2.,  6., 1., &! ,'VICTORY			    '&! 	 335	  
    -9.4800,  150.3500,    220.00, -1.,  5., 1., &! ,'GOODENOUGH		    '&! 	 336	  
    -9.5700,  149.0750,    640.00,  2.,  7., 1., &! ,'WAIOWA			    '&! 	 337	  
    -9.5200,  150.5300,    200.00, -1.,  5., 1., &! ,'IAMALELE  		    '&! 	 338	  
    -9.6200,  150.8800,    500.00,  6.,  5., 1., &! ,'DAWSONSTRAITGROUP 	    '&! 	 339	  
    -3.1250,  152.6420,    700.00, -1.,  1., 1., &! ,'LIHIR			    '&! 	 340	  
    -4.0800,  153.6500,    450.00,  7.,  5., 1., &! ,'AMBITLE			    '&! 	 341	  
    -5.8300,  154.9300,   2200.00, -1.,  5., 1., &! ,'TORE			    '&! 	 342	  
    -6.0920,  155.2250,   1544.00,  5.,  5., 1., &! ,'BILLYMITCHELL		    '&! 	 343	  
    -5.9200,  154.9800,   2715.00, -1.,  5., 1., &! ,'BALBI			    '&! 	 344	  
    -6.4420,  155.6080,   2210.00, -1.,  5., 1., &! ,'TAKUANGROUP		    '&! 	 345	  
    -6.1400,  155.1950,   1750.00,  1.,  6., 1., &! ,'BAGANA			    '&! 	 346	  
    -6.5200,  155.6200,   1887.00,  7.,  5., 1., &! ,'LOLORU			    '&! 	 347	  
    -8.7500,  157.0300,   -700.00, -1.,  5., 1., &! ,'KANAKEOKI 		    '&! 	 348	  
    -8.8300,  157.1700,      0.00, -1.,  5., 1., &! ,'COLEMANSEAMOUNT		    '&! 	 349	  
    -8.2920,  156.5200,    335.00,  2.,  5., 1., &! ,'SIMBO			    '&! 	 350	  
    -8.9200,  158.0300,   -240.00, -1.,  5., 1., &! ,'UNNAMED			    '&! 	 351	  
    -9.3500,  159.7300,   1000.00, -2.,  5., 1., &! ,'GALLEGO			    '&! 	 352	  
    -9.0200,  157.9500,    -20.00,  1.,  2., 1., &! ,'KAVACHI			    '&! 	 353	  
    -9.1300,  159.8200,    485.00,  3.,  7., 1., &! ,'SAVO			    '&! 	 354	  
   -10.3800,  165.8000,    851.00,  1.,  6., 1., &! ,'TINAKULA  		    '&! 	 355	  
   -13.6700,  167.6700,    411.00, -1.,  1., 1., &! ,'MOTLAV			    '&! 	 356	  
   -13.8000,  167.4700,    921.00,  2.,  5., 1., &! ,'SURETAMATAI		    '&! 	 357	  
   -14.4500,  168.0500,   1028.00, -1.,  1., 1., &! ,'MERELAVA  		    '&! 	 358	  
   -14.2700,  167.5000,    797.00,  2.,  6., 1., &! ,'GAUA			    '&! 	 359	  
   -15.4000,  167.8300,   1496.00,  1.,  1., 1., &! ,'AOBA			    '&! 	 360	  
   -16.2500,  168.1200,   1334.00,  1.,  2., 1., &! ,'AMBRYM			    '&! 	 361	  
   -16.5070,  168.3460,   1413.00,  1.,  3., 1., &! ,'LOPEVI			    '&! 	 362	  
   -16.6800,  168.3700,    833.00,  1.,  1., 1., &! ,'EPI			    '&! 	 363	  
   -16.8290,  168.5360,     -2.00,  2.,  1., 1., &! ,'KUWAE			    '&! 	 364	  
   -16.9920,  168.5920,    216.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	 365	  
   -17.4700,  168.3530,    594.00, -1.,  1., 1., &! ,'NORTHVATE 		    '&! 	 366	  
   -18.7500,  169.2300,    837.00,  3.,  1., 1., &! ,'TRAITORSHEAD		    '&! 	 367	  
   -19.5300,  169.4420,    361.00,  1.,  2., 1., &! ,'YASUR			    '&! 	 368	  
   -20.2000,  169.7800,    852.00, -2.,  1., 1., &! ,'ANEITYUM  		    '&! 	 369	  
   -20.9800,  170.2800,    -80.00,  2.,  1., 1., &! ,'EASTERNGEMINISEAMOUNT	    '&! 	 370	  
   -22.3300,  171.3200,    177.00,  2.,  5., 1., &! ,'MATTHEWISLAND		    '&! 	 371	  
   -22.4000,  172.0500,    297.00,  2.,  5., 1., &! ,'HUNTERISLAND		    '&! 	 372	  
   -25.7800,  168.6300,  -2400.00,  2.,  5., 1., &! ,'UNNAMED			    '&! 	 373	  
   -37.7700,  142.5000,   1011.00,  7.,  1., 1., &! ,'NEWERVOLCANICSPROV	    '&! 	 374	  
    13.4300,   94.2800,    710.00, -1.,  5., 1., &! ,'NARCONDUM 		    '&! 	 375	  
    12.2780,   93.8580,    354.00,  1.,  7., 1., &! ,'BARRENISLAND		    '&! 	 376	  
     5.4480,   95.6580,   1810.00,  3.,  5., 1., &! ,'SEULAWAHAGAM		    '&! 	 377	  
     4.9140,   96.3290,   2801.00,  1.,  6., 1., &! ,'PEUETSAGUE		    '&! 	 378	  
     4.7690,   96.8210,   2617.00,  2.,  6., 1., &! ,'TELONGBURNI		    '&! 	 379	  
     3.2300,   98.5200,   2212.00,  3.,  5., 1., &! ,'SIBAYAK			    '&! 	 380	  
     3.1700,   98.3920,   2460.00, -1.,  5., 1., &! ,'SINABUNG  		    '&! 	 381	  
     2.5800,   98.8300,   2157.00, -1.,  5., 1., &! ,'TOBA			    '&! 	 382	  
     2.1580,   98.9300,   1505.00, -2.,  5., 1., &! ,'IMUN			    '&! 	 383	  
     1.4780,   99.2090,   1862.00, -2.,  5., 1., &! ,'LUBUKRAYA 		    '&! 	 384	  
     1.5560,   99.2550,   1819.00, -2.,  5., 1., &! ,'SIBUALBUALI		    '&! 	 385	  
     0.6860,   99.5390,   2145.00,  2.,  6., 1., &! ,'SORIKMARAPI		    '&! 	 386	  
     0.0800,  100.2000,      0.00, -2.,  5., 1., &! ,'SARIKGAJAH		    '&! 	 387	  
     0.0790,   99.9800,   2919.00, -1.,  5., 1., &! ,'TALAKMAU  		    '&! 	 388	  
    -0.3810,  100.4730,   2891.00,  1.,  6., 1., &! ,'MARAPI			    '&! 	 389	  
    -0.4330,  100.3170,   2438.00,  2.,  6., 1., &! ,'TANDIKAT  		    '&! 	 390	  
    -0.9780,  100.6790,   2597.00,  1.,  6., 1., &! ,'TALANG			    '&! 	 391	  
    -2.2740,  101.4830,   2151.00, -1.,  5., 1., &! ,'KUNYIT			    '&! 	 392	  
    -2.3300,  101.6000,   2021.00, -1.,  5., 1., &! ,'HUTAPANJANG		    '&! 	 393	  
    -1.6970,  101.2640,   3800.00,  1.,  6., 1., &! ,'KERINCI			    '&! 	 394	  
    -2.4140,  101.7280,   2507.00,  2.,  6., 1., &! ,'SUMBING			    '&! 	 395	  
    -2.8200,  102.0200,      0.00, -1.,  5., 1., &! ,'PENDAN			    '&! 	 396	  
    -2.8200,  102.1800,   1958.00, -1.,  1., 1., &! ,'BELIRANGBERITI		    '&! 	 397	  
    -3.3800,  102.3700,   2467.00, -1.,  1., 1., &! ,'DAUNBUKIT 		    '&! 	 398	  
    -3.5200,  102.6200,   1952.00,  1.,  6., 1., &! ,'KABA			    '&! 	 399	  
    -4.2700,  103.3000,   2817.00, -2.,  5., 1., &! ,'PATAH			    '&! 	 400	  
    -4.0300,  103.1300,   3173.00,  2.,  6., 1., &! ,'DEMPO			    '&! 	 401	  
    -4.2200,  103.6200,   2055.00, -1.,  5., 1., &! ,'LUMUTBALAIBUKIT		    '&! 	 402	  
    -4.8300,  103.9200,   1881.00, -2.,  5., 1., &! ,'RANAU			    '&! 	 403	  
    -4.4300,  103.6700,   1899.00,  2.,  5., 1., &! ,'BESAR			    '&! 	 404	  
    -5.1200,  104.3200,   1719.00, -1.,  5., 1., &! ,'SEKINCAUBELIRANG  	    '&! 	 405	  
    -5.2500,  104.2700,   1000.00,  2.,  5., 1., &! ,'SUOH			    '&! 	 406	  
    -5.3500,  104.6000,   1040.00, -1.,  5., 1., &! ,'HULUBELU  		    '&! 	 407	  
    -5.7800,  105.6250,   1281.00, -1.,  5., 1., &! ,'RAJABASA  		    '&! 	 408	  
    -6.1020,  105.4230,    813.00,  1.,  6., 1., &! ,'KRAKATAU  		    '&! 	 409	  
    -6.3420,  105.9750,   1346.00, -1.,  5., 1., &! ,'PULOSARI  		    '&! 	 410	  
    -6.2700,  106.0420,   1778.00, -2.,  5., 1., &! ,'KARANG			    '&! 	 411	  
    -6.7500,  106.7000,   1699.00,  2.,  5., 1., &! ,'PERBAKTIGAGAK		    '&! 	 412	  
    -6.7200,  106.7300,   2211.00,  2.,  6., 1., &! ,'SALAK			    '&! 	 413	  
    -6.7800,  106.9800,   2958.00,  2.,  7., 1., &! ,'GEDE			    '&! 	 414	  
    -7.1600,  107.4000,   2434.00, -1.,  5., 1., &! ,'PATUHA			    '&! 	 415	  
    -7.1300,  107.6500,   2343.00, -2.,  5., 1., &! ,'MALABAR			    '&! 	 416	  
    -7.2080,  107.6300,   2182.00, -1.,  5., 1., &! ,'WAYANGWINDU		    '&! 	 417	  
    -6.7700,  107.6000,   2084.00,  2.,  6., 1., &! ,'TANGKUBANPARAHU		    '&! 	 418	  
    -7.3200,  107.7300,   2665.00,  1.,  6., 1., &! ,'PAPANDAYAN		    '&! 	 419	  
    -7.2300,  107.7200,   2608.00, -1.,  5., 1., &! ,'KENDANG			    '&! 	 420	  
    -6.7700,  107.9500,   1684.00, -1.,  5., 1., &! ,'TAMPOMAS  		    '&! 	 421	  
    -7.1430,  107.8400,   2249.00,  3.,  6., 1., &! ,'GUNTUR			    '&! 	 422	  
    -7.2500,  108.0580,   2168.00,  2.,  3., 1., &! ,'GALUNGGUNG		    '&! 	 423	  
    -7.2080,  108.0700,   2201.00, -1.,  1., 1., &! ,'TALAGABODAS		    '&! 	 424	  
    -7.1200,  108.0800,   1155.00, -1.,  5., 1., &! ,'KARAHAKAWAH		    '&! 	 425	  
    -6.8920,  108.4000,   3078.00,  2.,  6., 1., &! ,'CEREME			    '&! 	 426	  
    -7.2420,  109.2080,   3428.00,  2.,  2., 1., &! ,'SLAMET			    '&! 	 427	  
    -7.2000,  109.9200,   2565.00,  2.,  6., 1., &! ,'DIENGVOLCCOMPLEX  	    '&! 	 428	  
    -7.3000,  109.9920,   3136.00,  2.,  6., 1., &! ,'SUNDORO			    '&! 	 429	  
    -7.3840,  110.0700,   3371.00,  4.,  5., 1., &! ,'SUMBING			    '&! 	 430	  
    -7.3700,  110.4000,   1894.00, -1.,  5., 1., &! ,'TELOMOYO  		    '&! 	 431	  
    -7.1800,  110.3300,   2050.00, -1.,  5., 1., &! ,'UNGARAN			    '&! 	 432	  
    -7.4500,  110.4300,   3145.00,  4.,  2., 1., &! ,'MERBABU			    '&! 	 433	  
    -6.6200,  110.8800,   1625.00,  7.,  1., 1., &! ,'MURIA			    '&! 	 434	  
    -7.5420,  110.4420,   2968.00,  1.,  1., 1., &! ,'MERAPI			    '&! 	 435	  
    -7.6250,  111.1920,   3265.00,  3.,  6., 1., &! ,'LAWU			    '&! 	 436	  
    -7.8080,  111.7580,   2563.00, -1.,  5., 1., &! ,'WILIS			    '&! 	 437	  
    -7.9200,  112.4500,   2651.00, -1.,  5., 1., &! ,'KAWIBUTAK 		    '&! 	 438	  
    -7.9300,  112.3080,   1731.00,  1.,  7., 1., &! ,'KELUT			    '&! 	 439	  
    -7.6200,  112.6300,   1653.00, -1.,  5., 1., &! ,'PENANGGUNGAN		    '&! 	 440	  
    -8.0200,  112.6800,    680.00, -1.,  5., 1., &! ,'MALANGPLAIN		    '&! 	 441	  
    -7.7250,  112.5800,   3339.00,  2.,  6., 1., &! ,'ARJUNOWELIRANG		    '&! 	 442	  
    -8.1080,  112.9200,   3676.00,  1.,  6., 1., &! ,'SEMERU			    '&! 	 443	  
    -7.9420,  112.9500,   2329.00,  1.,  6., 1., &! ,'TENGGERCALDERA		    '&! 	 444	  
    -7.7300,  113.5800,    539.00, -2.,  5., 1., &! ,'LURUS			    '&! 	 445	  
    -7.9790,  113.3420,   1651.00,  3.,  2., 1., &! ,'LAMONGAN  		    '&! 	 446	  
    -7.9700,  113.5700,   3088.00, -1.,  5., 1., &! ,'IYANGARGAPURA		    '&! 	 447	  
    -8.1250,  114.0420,   3332.00,  1.,  6., 1., &! ,'RAUNG			    '&! 	 448	  
    -7.8500,  114.3700,   1247.00, -2.,  5., 1., &! ,'BALURAN			    '&! 	 449	  
    -8.0580,  114.2420,   2799.00,  2.,  6., 1., &! ,'IJEN			    '&! 	 450	  
    -8.2800,  115.1300,   2276.00, -1.,  5., 1., &! ,'BRATAN			    '&! 	 451	  
    -8.2420,  115.3750,   1717.00,  1.,  6., 1., &! ,'BATUR			    '&! 	 452	  
    -8.3420,  115.5080,   3142.00,  2.,  5., 1., &! ,'AGUNG			    '&! 	 453	  
    -8.4200,  116.4700,   3726.00,  1.,  6., 1., &! ,'RINJANI			    '&! 	 454	  
    -8.2500,  118.0000,   2850.00,  2.,  5., 1., &! ,'TAMBORA			    '&! 	 455	  
    -8.2000,  119.0700,   1949.00,  2.,  3., 1., &! ,'SANGEANGAPI		    '&! 	 456	  
    -8.7200,  120.0200,    903.00, -1.,  5., 1., &! ,'SANOWAI			    '&! 	 457	  
    -8.6200,  120.5200,   2350.00,  2.,  5., 1., &! ,'RANAKAH			    '&! 	 458	  
    -8.6800,  120.4800,   1675.00, -1.,  5., 1., &! ,'POCOLEOK  		    '&! 	 459	  
    -8.8750,  120.9500,   2245.00,  7.,  5., 1., &! ,'INIERIE			    '&! 	 460	  
    -8.7300,  120.9800,   1559.00,  1.,  6., 1., &! ,'INIELIKA  		    '&! 	 461	  
    -8.8200,  121.1800,   2124.00,  2.,  6., 1., &! ,'EBULOBO			    '&! 	 462	  
    -8.8970,  121.6450,    637.00,  2.,  2., 1., &! ,'IYA			    '&! 	 463	  
    -8.7920,  121.7700,   1500.00, -1.,  5., 1., &! ,'SUKARIACALDERA		    '&! 	 464	  
    -8.7200,  121.7800,    750.00, -2.,  5., 1., &! ,'NDETENAPU 		    '&! 	 465	  
    -8.7700,  121.8200,   1639.00,  2.,  6., 1., &! ,'KELIMUTU  		    '&! 	 466	  
    -8.3200,  121.7080,    875.00,  2.,  7., 1., &! ,'PALUWEH			    '&! 	 467	  
    -8.6700,  122.4500,   1703.00,  1.,  6., 1., &! ,'EGON			    '&! 	 468	  
    -8.4780,  122.6710,   1100.00, -1.,  5., 1., &! ,'ILIMUDA			    '&! 	 469	  
    -8.5420,  122.7750,   1703.00,  1.,  6., 1., &! ,'LEWOTOBI  		    '&! 	 470	  
    -8.3580,  122.8420,   1117.00,  1.,  6., 1., &! ,'LEROBOLENG		    '&! 	 471	  
    -8.3420,  123.2580,   1659.00,  2.,  2., 1., &! ,'ILIBOLENG 		    '&! 	 472	  
    -8.2720,  123.5050,   1423.00,  2.,  6., 1., &! ,'LEWOTOLO  		    '&! 	 473	  
    -8.5500,  123.3800,   1018.00, -1.,  1., 1., &! ,'ILILABALEKAN		    '&! 	 474	  
    -8.5300,  123.5700,   1018.00,  2.,  2., 1., &! ,'ILIWERUNG 		    '&! 	 475	  
    -7.7920,  123.5790,    748.00,  1.,  2., 1., &! ,'TARABATU  		    '&! 	 476	  
    -8.5080,  124.1300,    862.00,  2.,  6., 1., &! ,'SIRUNG			    '&! 	 477	  
    -7.5300,  123.9500,  -3800.00, -2.,  6., 1., &! ,'YERSEY			    '&! 	 478	  
    -6.6200,  124.2200,  -2850.00, -2.,  6., 1., &! ,'EMPEROROFCHINA		    '&! 	 479	  
    -6.6000,  124.6750,  -2285.00, -2.,  6., 1., &! ,'NIEUWERKERK		    '&! 	 480	  
    -6.6420,  126.6500,    282.00,  5.,  7., 1., &! ,'GUNUNGAPIWETAR		    '&! 	 481	  
    -7.1250,  128.6750,    868.00,  3.,  6., 1., &! ,'WURLALI			    '&! 	 482	  
    -6.9200,  129.1250,    655.00,  2.,  7., 1., &! ,'TEON			    '&! 	 483	  
    -6.7300,  129.5000,    781.00,  2.,  6., 1., &! ,'NILA			    '&! 	 484	  
    -6.3000,  130.0000,    641.00,  2.,  6., 1., &! ,'SERUA			    '&! 	 485	  
    -5.5300,  130.2920,    282.00, -1.,  5., 1., &! ,'MANUK			    '&! 	 486	  
    -4.5250,  129.8710,    640.00,  2.,  6., 1., &! ,'BANDAAPI  		    '&! 	 487	  
    -0.1700,  121.6080,    507.00,  2.,  7., 1., &! ,'COLO[UNAUNA]		    '&! 	 488	  
     0.7500,  124.4200,   1795.00,  3.,  5., 1., &! ,'AMBANG			    '&! 	 489	  
     1.1080,  124.7300,   1784.00,  1.,  6., 1., &! ,'SOPUTAN			    '&! 	 490	  
     1.1300,  124.7580,   1549.00, -1.,  5., 1., &! ,'SEMPU			    '&! 	 491	  
     1.2300,  124.8300,   1202.00, -1.,  5., 1., &! ,'TONDANOCALDERA		    '&! 	 492	  
     1.3580,  124.7920,   1580.00,  1.,  6., 1., &! ,'LOKONEMPUNG		    '&! 	 493	  
     1.3580,  124.8580,   1324.00,  2.,  6., 1., &! ,'MAHAWU			    '&! 	 494	  
     1.4700,  125.0300,   1995.00, -1.,  5., 1., &! ,'KLABAT			    '&! 	 495	  
     1.5200,  125.2000,   1149.00,  3.,  6., 1., &! ,'TONGKOKO  		    '&! 	 496	  
     2.3000,  125.3700,    725.00,  1.,  6., 1., &! ,'RUANG			    '&! 	 497	  
     2.7800,  125.4000,   1784.00,  1.,  6., 1., &! ,'KARANGETANG[APISIAU]	    '&! 	 498	  
     3.1380,  125.4910,     -5.00,  2.,  6., 1., &! ,'BANUAWUHU 		    '&! 	 499	  
     3.6700,  125.5000,   1320.00,  1.,  6., 1., &! ,'AWU			    '&! 	 500	  
     3.9700,  124.1700,  -5000.00, -2.,  6., 1., &! ,'UNNAMED			    '&! 	 501	  
     1.8300,  127.8300,    318.00, -1.,  1., 1., &! ,'TARAKAN			    '&! 	 502	  
     1.6800,  127.8800,   1335.00,  1.,  6., 1., &! ,'DUKONO			    '&! 	 503	  
     1.6300,  127.6700,   1035.00, -1.,  5., 1., &! ,'TOBARU			    '&! 	 504	  
     1.4880,  127.6300,   1325.00,  1.,  6., 1., &! ,'IBU			    '&! 	 505	  
     1.3800,  127.5300,   1635.00,  1.,  6., 1., &! ,'GAMKONORA 		    '&! 	 506	  
     1.0800,  127.4200,   1130.00, -1.,  5., 1., &! ,'JAILOLO			    '&! 	 507	  
     0.9000,  127.3200,    630.00, -1.,  1., 1., &! ,'HIRI			    '&! 	 508	  
     1.2500,  127.4700,    979.00, -1.,  5., 1., &! ,'TODOKORANU		    '&! 	 509	  
     0.6580,  127.4000,   1730.00, -1.,  5., 1., &! ,'TIDORE			    '&! 	 510	  
     0.5700,  127.4000,    308.00, -1.,  5., 1., &! ,'MARE			    '&! 	 511	  
     0.4500,  127.4000,    950.00, -1.,  5., 1., &! ,'MOTI			    '&! 	 512	  
     0.8000,  127.3300,   1715.00,  1.,  6., 1., &! ,'GAMALAMA  		    '&! 	 513	  
     0.0700,  127.4200,    422.00, -1.,  5., 1., &! ,'TIGALALU  		    '&! 	 514	  
    -0.5300,  127.4800,   1030.00, -1.,  5., 1., &! ,'AMASING			    '&! 	 515	  
    -0.7700,  127.7200,    900.00, -1.,  5., 1., &! ,'BIBINOI			    '&! 	 516	  
     0.3200,  127.4000,   1357.00,  2.,  7., 1., &! ,'MAKIAN			    '&! 	 517	  
     4.4000,  117.8800,    531.00, -2.,  1., 1., &! ,'BOMBALAI  		    '&! 	 518	  
     6.0130,  121.0570,    811.00,  3.,  1., 1., &! ,'JOLO			    '&! 	 519	  
     6.1130,  124.8920,   1824.00,  5.,  5., 1., &! ,'PARKER			    '&! 	 520	  
     5.4000,  125.3750,    862.00, -1.,  5., 1., &! ,'BALUT			    '&! 	 521	  
     6.3700,  125.0700,   2286.00,  6.,  5., 1., &! ,'MATUTUM			    '&! 	 522	  
     7.3820,  126.0470,   1080.00,  6.,  5., 1., &! ,'LEONARDRANGE		    '&! 	 523	  
     6.9890,  125.2690,   2938.00, -1.,  5., 1., &! ,'APO			    '&! 	 524	  
     7.6470,  124.3200,   1940.00,  3.,  5., 1., &! ,'MAKATURING		    '&! 	 525	  
     7.6500,  124.4500,   2338.00, -2.,  5., 1., &! ,'LATUKAN			    '&! 	 526	  
     7.9500,  124.8000,   2824.00, -2.,  5., 1., &! ,'KALATUNGAN		    '&! 	 527	  
     7.7000,  124.5000,   2815.00,  3.,  6., 1., &! ,'RAGANG			    '&! 	 528	  
     8.2200,  123.6300,   2404.00, -1.,  5., 1., &! ,'MALINDANG 		    '&! 	 529	  
     8.7700,  124.9800,   2450.00, -2.,  1., 1., &! ,'BALATUKAN 		    '&! 	 530	  
     7.8770,  125.0680,    646.00,  3.,  5., 1., &! ,'MUSUAN			    '&! 	 531	  
     9.2030,  124.6730,   1552.00,  2.,  5., 1., &! ,'CAMIGUIN  		    '&! 	 532	  
     9.5930,  125.5200,    524.00, -1.,  5., 1., &! ,'PACO			    '&! 	 533	  
     9.2500,  123.1700,   1862.00, -1.,  5., 1., &! ,'CUERNOSDENEGROS		    '&! 	 534	  
    10.4120,  123.1320,   2435.00,  1.,  6., 1., &! ,'KANLAON			    '&! 	 535	  
    10.6500,  123.2500,   1885.00, -1.,  5., 1., &! ,'MANDALAGAN		    '&! 	 536	  
    10.7700,  123.2300,   1510.00, -1.,  5., 1., &! ,'SILAY			    '&! 	 537	  
    10.2870,  125.2210,    945.00,  3.,  5., 1., &! ,'CABALIAN  		    '&! 	 538	  
    10.8960,  125.8700,    860.00, -1.,  5., 1., &! ,'MAHAGNAO  		    '&! 	 539	  
    11.5230,  124.5350,   1301.00,  2.,  5., 1., &! ,'BILIRAN			    '&! 	 540	  
    12.7700,  124.0500,   1565.00,  1.,  6., 1., &! ,'BULUSAN			    '&! 	 541	  
    13.0500,  123.9580,   1102.00, -1.,  5., 1., &! ,'POCDOLMOUNTAINS		    '&! 	 542	  
    13.3200,  123.6000,   1328.00, -1.,  5., 1., &! ,'MASARAGA  		    '&! 	 543	  
    13.2570,  123.6850,   2462.00,  1.,  7., 1., &! ,'MAYON			    '&! 	 544	  
    13.4570,  123.4570,   1196.00, -1.,  5., 1., &! ,'IRIGA			    '&! 	 545	  
    13.6580,  123.3800,   1966.00, -1.,  5., 1., &! ,'ISAROG			    '&! 	 546	  
    13.2400,  122.0180,   1157.00, -1.,  5., 1., &! ,'MALINDIG  		    '&! 	 547	  
    14.0700,  121.4800,   2158.00, -1.,  5., 1., &! ,'BANAHAW			    '&! 	 548	  
    14.1200,  121.3000,   1090.00,  6.,  5., 1., &! ,'SANPABLOVOLCFIELD 	    '&! 	 549	  
    14.0020,  120.9930,    311.00,  2.,  7., 1., &! ,'TAAL			    '&! 	 550	  
    14.5200,  120.4700,   1388.00,  7.,  5., 1., &! ,'MARIVELES 		    '&! 	 551	  
    14.7200,  120.4000,   1253.00, -2.,  5., 1., &! ,'NATIB			    '&! 	 552	  
    15.1300,  120.3500,   1486.00,  2.,  8., 1., &! ,'PINATUBO  		    '&! 	 553	  
    15.2000,  120.7420,   1026.00, -2.,  5., 1., &! ,'ARAYAT			    '&! 	 554	  
    15.8280,  120.8050,    376.00, -1.,  5., 1., &! ,'AMORONG			    '&! 	 555	  
    16.3300,  120.5500,   2260.00, -2.,  5., 1., &! ,'SANTOTOMAS		    '&! 	 556	  
    17.1470,  120.9800,   1865.00, -1.,  5., 1., &! ,'PATOC			    '&! 	 557	  
    17.3200,  121.1000,   2329.00, -1.,  5., 1., &! ,'AMBALATUNGANGROUP 	    '&! 	 558	  
    14.4200,  121.2700,    743.00, -1.,  5., 1., &! ,'LAGUNACALDERA		    '&! 	 559	  
    18.2220,  122.1230,   1133.00,  3.,  5., 1., &! ,'CAGUA			    '&! 	 560	  
    18.8300,  121.8600,    712.00,  3.,  5., 1., &! ,'CAMIGUINDEBABUYANES	    '&! 	 561	  
    19.0770,  122.2020,    228.00,  2.,  5., 1., &! ,'DIDICAS			    '&! 	 562	  
    19.5230,  121.9400,   1080.00,  2.,  6., 1., &! ,'BABUYANCLARO		    '&! 	 563	  
    20.3300,  121.7500,    -24.00,  3.,  5., 1., &! ,'UNNAMED			    '&! 	 564	  
    20.4690,  122.0100,   1009.00,  6.,  5., 1., &! ,'IRAYA			    '&! 	 565	  
    19.7000,  110.1000,      0.00,  2.,  5., 1., &! ,'HAINANDAO 		    '&! 	 566	  
    20.7800,  110.1700,    259.00, -1.,  5., 1., &! ,'LEIZHOUBANDAO		    '&! 	 567	  
    15.3800,  109.1200,    181.00, -1.,  5., 1., &! ,'CULAOREGROUP		    '&! 	 568	  
    14.9300,  108.0000,    800.00, -2.,  1., 1., &! ,'TOROENGPRONG		    '&! 	 569	  
    11.6000,  108.2000,   1000.00, -2.,  5., 1., &! ,'HAUTDONGNAI		    '&! 	 570	  
    10.8000,  107.2000,    392.00, -2.,  5., 1., &! ,'BASDONGNAI		    '&! 	 571	  
    10.1580,  109.0140,    -20.00,  2.,  1., 1., &! ,'CENDRESILEDES		    '&! 	 572	  
     9.8300,  109.0500,      0.00, -1.,  5., 1., &! ,'VETERAN			    '&! 	 573	  
    20.9200,   95.2500,   1518.00,  7.,  5., 1., &! ,'POPA			    '&! 	 574	  
    22.2800,   95.1000,    385.00, -2.,  5., 1., &! ,'LOWERCHINDWIN		    '&! 	 575	  
    22.7000,   95.9800,    507.00, -1.,  5., 1., &! ,'SINGUPLATEAU		    '&! 	 576	  
    25.2300,   98.5000,   2865.00,  7.,  5., 1., &! ,'TENGCHONG 		    '&! 	 577	  
    19.1700,  132.2500,    -10.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	 578	  
    20.9300,  134.7500,  -6000.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	 579	  
    21.8300,  121.1800,   -115.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	 580	  
    24.8500,  121.9200,    401.00,  4.,  5., 1., &! ,'KUEISHANTAO		    '&! 	 581	  
    24.0000,  121.8300,      0.00,  3.,  5., 1., &! ,'UNNAMED			    '&! 	 582	  
    25.4000,  122.2000,   -100.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	 583	  
    26.1800,  122.4580,   -418.00, -2.,  5., 1., &! ,'ZENGYU			    '&! 	 584	  
    24.5580,  124.0000,   -200.00,  2.,  5., 1., &! ,'IRIOMOTEJIMA		    '&! 	 585	  
    28.7970,  128.9970,    495.00,  3.,  5., 1., &! ,'YOKOATEJIMA		    '&! 	 586	  
    29.4610,  129.5970,    584.00, -2.,  5., 1., &! ,'AKUSEKIJIMA		    '&! 	 587	  
    27.8770,  128.2240,    212.00,  2.,  6., 1., &! ,'IWOTORISHIMA		    '&! 	 588	  
    29.6350,  129.7160,    799.00,  1.,  6., 1., &! ,'SUWANOSEJIMA		    '&! 	 589	  
    29.8790,  129.6250,    301.00, -2.,  5., 1., &! ,'KOGAJAJIMA		    '&! 	 590	  
    29.9640,  129.9270,    628.00,  6.,  5., 1., &! ,'KUCHINOSHIMA		    '&! 	 591	  
    29.8560,  129.8590,    979.00,  2.,  5., 1., &! ,'NAKANOSHIMA		    '&! 	 592	  
    30.4400,  130.2190,    657.00,  2.,  7., 1., &! ,'KUCHINOERABUJIMA  	    '&! 	 593	  
    30.7890,  130.3080,    704.00,  1.,  6., 1., &! ,'KIKAI			    '&! 	 594	  
    31.2200,  130.5700,    922.00,  6.,  5., 1., &! ,'IBUSUKIVOLCFIELD  	    '&! 	 595	  
    31.7680,  130.5940,     15.00,  7.,  1., 1., &! ,'SUMIYOSHIIKE		    '&! 	 596	  
    31.5850,  130.6570,   1117.00,  1.,  6., 1., &! ,'SAKURAJIMA		    '&! 	 597	  
    32.6530,  128.8510,    317.00,  7.,  1., 1., &! ,'FUKUEJIMA 		    '&! 	 598	  
    31.9310,  130.8640,   1700.00,  1.,  6., 1., &! ,'KIRISHIMA 		    '&! 	 599	  
    32.7570,  130.2940,   1500.00,  2.,  6., 1., &! ,'UNZEN			    '&! 	 600	  
    32.8810,  131.1060,   1592.00,  1.,  2., 1., &! ,'ASO			    '&! 	 601	  
    33.0830,  131.2510,   1791.00,  2.,  2., 1., &! ,'KUJU			    '&! 	 602	  
    33.2800,  131.4320,   1584.00,  6.,  5., 1., &! ,'TSURUMI			    '&! 	 603	  
    34.5000,  131.6000,    641.00,  7.,  1., 1., &! ,'ABU			    '&! 	 604	  
    35.1300,  132.6200,   1126.00,  6.,  5., 1., &! ,'SANBE			    '&! 	 605	  
    36.1760,  133.3340,    151.00, -1.,  1., 1., &! ,'OKIDOGO			    '&! 	 606	  
    34.9000,  139.0980,   1406.00,  2.,  1., 1., &! ,'IZUTOBU			    '&! 	 607	  
    35.2300,  139.0240,   1438.00,  6.,  5., 1., &! ,'HAKONE			    '&! 	 608	  
    36.1000,  138.3000,   2530.00,  6.,  5., 1., &! ,'KITAYATSUGATAKE		    '&! 	 609	  
    35.3580,  138.7310,   3776.00,  4.,  7., 1., &! ,'FUJI			    '&! 	 610	  
    35.8900,  137.4800,   3063.00,  2.,  5., 1., &! ,'ONTAKE			    '&! 	 611	  
    36.1520,  136.7740,   2702.00,  5.,  7., 1., &! ,'HAKUSAN			    '&! 	 612	  
    36.1030,  137.5570,   3026.00,  7.,  5., 1., &! ,'NORIKURA  		    '&! 	 613	  
    36.4080,  137.5940,   2924.00,  0.,  1., 1., &! ,'WASHIBAKUMONOTAIRA	    '&! 	 614	  
    36.2240,  137.5900,   2455.00,  2.,  5., 1., &! ,'YAKEDAKE  		    '&! 	 615	  
    36.5680,  137.5930,   2621.00,  3.,  5., 1., &! ,'TATEYAMA  		    '&! 	 616	  
    36.9180,  138.0390,   2400.00,  2.,  6., 1., &! ,'NIIGATAYAKEYAMA		    '&! 	 617	  
    36.8880,  138.1200,   2446.00,  7.,  5., 1., &! ,'MYOKO			    '&! 	 618	  
    36.4030,  138.5260,   2568.00,  1.,  6., 1., &! ,'ASAMA			    '&! 	 619	  
    36.6880,  138.5190,   2041.00, -2.,  1., 1., &! ,'SHIGA			    '&! 	 620	  
    36.4740,  138.8810,   1449.00,  6.,  5., 1., &! ,'HARUNA			    '&! 	 621	  
    36.6200,  138.5350,   2171.00,  2.,  6., 1., &! ,'KUSATSUSHIRANE		    '&! 	 622	  
    36.9520,  139.2890,   2356.00,  5.,  5., 1., &! ,'HIUCHI			    '&! 	 623	  
    36.5570,  139.1960,   1828.00, -2.,  5., 1., &! ,'AKAGI			    '&! 	 624	  
    36.7620,  139.4940,   2486.00, -2.,  5., 1., &! ,'NANTAI			    '&! 	 625	  
    36.7920,  139.5100,   2367.00,  7.,  5., 1., &! ,'OMANAGOGROUP		    '&! 	 626	  
    36.8970,  139.7800,   1795.00,  7.,  5., 1., &! ,'TAKAHARA  		    '&! 	 627	  
    36.7960,  139.3790,   2578.00,  2.,  6., 1., &! ,'NIKKOSHIRANE		    '&! 	 628	  
    37.4500,  139.5790,   1100.00,  7.,  1., 1., &! ,'NUMAZAWA  		    '&! 	 629	  
    37.1220,  139.9660,   1915.00,  2.,  6., 1., &! ,'NASU			    '&! 	 630	  
    37.5980,  140.0760,   1819.00,  3.,  6., 1., &! ,'BANDAI			    '&! 	 631	  
    37.6440,  140.2860,   1718.00,  2.,  6., 1., &! ,'ADATARA			    '&! 	 632	  
    37.7320,  140.2480,   2035.00,  2.,  6., 1., &! ,'AZUMA			    '&! 	 633	  
    38.6060,  140.1780,    516.00, -1.,  5., 1., &! ,'HIJIORI			    '&! 	 634	  
    38.1410,  140.4430,   1841.00,  2.,  6., 1., &! ,'ZAO			    '&! 	 635	  
    38.7330,  140.7320,    470.00,  6.,  5., 1., &! ,'NARUGO			    '&! 	 636	  
    38.9580,  140.7920,   1628.00,  2.,  6., 1., &! ,'KURIKOMA  		    '&! 	 637	  
    39.0960,  140.0520,   2233.00,  2.,  6., 1., &! ,'CHOKAI			    '&! 	 638	  
    39.7580,  140.8030,   1637.00,  2.,  6., 1., &! ,'AKITAKOMAGATAKE		    '&! 	 639	  
    39.8500,  141.0040,   2041.00,  2.,  5., 1., &! ,'IWATE			    '&! 	 640	  
    39.9550,  140.8570,   1614.00,  7.,  5., 1., &! ,'HACHIMANTAI		    '&! 	 641	  
    39.9500,  139.7300,    291.00,  7.,  1., 1., &! ,'MEGATA			    '&! 	 642	  
    39.9610,  140.7610,   1366.00,  2.,  5., 1., &! ,'AKITAYAKEYAMA		    '&! 	 643	  
    40.4700,  140.9200,   1159.00,  6.,  5., 1., &! ,'TOWADA			    '&! 	 644	  
    40.6530,  140.3070,   1625.00,  3.,  6., 1., &! ,'IWAKI			    '&! 	 645	  
    40.6560,  140.8810,   1585.00,  5.,  5., 1., &! ,'HAKKODAGROUP		    '&! 	 646	  
    41.2760,  141.1240,    879.00,  4.,  5., 1., &! ,'OSOREYAMA 		    '&! 	 647	  
    34.5170,  139.2830,    508.00,  7.,  5., 1., &! ,'TOSHIMA			    '&! 	 648	  
    34.7210,  139.3980,    764.00,  2.,  6., 1., &! ,'OSHIMA			    '&! 	 649	  
    34.3930,  139.2730,    432.00,  6.,  5., 1., &! ,'NIIJIMA			    '&! 	 650	  
    34.2160,  139.1560,    572.00,  6.,  5., 1., &! ,'KOZUSHIMA 		    '&! 	 651	  
    33.8710,  139.6050,    851.00,  7.,  5., 1., &! ,'MIKURAJIMA		    '&! 	 652	  
    33.4000,  139.6800,   -107.00, -2.,  5., 1., &! ,'KUROSEHOLE		    '&! 	 653	  
    34.0790,  139.5290,    815.00,  1.,  3., 1., &! ,'MIYAKEJIMA		    '&! 	 654	  
    33.1300,  139.7690,    854.00,  5.,  1., 1., &! ,'HACHIJOJIMA		    '&! 	 655	  
    32.1000,  139.8500,    360.00, -2.,  5., 1., &! ,'MYOJINKNOLL		    '&! 	 656	  
    32.4540,  139.7620,    423.00,  4.,  5., 1., &! ,'AOGASHIMA 		    '&! 	 657	  
    31.8800,  139.9200,     11.00,  2.,  6., 1., &! ,'BAYONNAISEROCKS		    '&! 	 658	  
    31.4360,  140.0540,    136.00,  2.,  5., 1., &! ,'SMITHROCK 		    '&! 	 659	  
    29.7890,  140.3450,     99.00, -2.,  1., 1., &! ,'SOFUGAN			    '&! 	 660	  
    28.6000,  140.6300,  -1418.00, -1.,  1., 1., &! ,'SUIYOSEAMOUNT		    '&! 	 661	  
    28.3200,  140.5700,   -920.00, -1.,  1., 1., &! ,'MOKUYOSEAMOUNT		    '&! 	 662	  
    27.6800,  140.8000,   -860.00, -1.,  1., 1., &! ,'DOYOSEAMOUNT		    '&! 	 663	  
    27.2740,  140.8820,     38.00,  2.,  5., 1., &! ,'NISHINOSHIMA		    '&! 	 664	  
    26.6700,  141.0000,   -162.00, -1.,  5., 1., &! ,'KAIKATASEAMOUNT		    '&! 	 665	  
    30.4800,  140.3060,    394.00,  1.,  1., 1., &! ,'TORISHIMA 		    '&! 	 666	  
    26.1300,  144.4800,  -3200.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	 667	  
    26.1220,  141.1020,   -103.00,  2.,  1., 1., &! ,'KAITOKUSEAMOUNT		    '&! 	 668	  
    25.4240,  141.2840,    792.00,  2.,  1., 1., &! ,'KITAIWOJIMA		    '&! 	 669	  
    24.4140,  141.4190,    -73.00,  2.,  1., 1., &! ,'KITAFUKUTOKUTAI		    '&! 	 670	  
    24.7540,  141.2900,    161.00,  1.,  6., 1., &! ,'IOTO[IWOJIMA]		    '&! 	 671	  
    23.4970,  141.9400,    -30.00,  2.,  1., 1., &! ,'MINAMIHIYOSHI		    '&! 	 672	  
    23.0750,  142.3080,   -391.00, -1.,  1., 1., &! ,'NIKKO			    '&! 	 673	  
    21.9300,  143.4700,   -217.00,  2.,  1., 1., &! ,'FUKUJIN			    '&! 	 674	  
    21.7650,  143.7100,   -598.00,  2.,  1., 1., &! ,'KASUGA			    '&! 	 675	  
    21.6000,  143.6370,   -274.00, -1.,  1., 1., &! ,'MINAMIKASUGA		    '&! 	 676	  
    21.4850,  144.0430,  -1535.00, -1.,  1., 1., &! ,'NWEIFUKU  		    '&! 	 677	  
    21.3240,  144.1940,   -323.00, -1.,  1., 1., &! ,'DAIKOKU			    '&! 	 678	  
    21.0000,  142.9000,      0.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	 679	  
    20.3000,  143.2000,      0.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	 680	  
    24.2800,  141.4850,    -14.00,  1.,  6., 1., &! ,'FUKUTOKUOKANOBA		    '&! 	 681	  
    20.4200,  145.0300,   -137.00,  1.,  6., 1., &! ,'AHYI			    '&! 	 682	  
    20.1300,  145.1000,     -8.00,  2.,  5., 1., &! ,'SUPPLYREEF		    '&! 	 683	  
    20.0200,  145.2200,    227.00, -1.,  1., 1., &! ,'MAUGISLANDS		    '&! 	 684	  
    20.5380,  144.8960,    360.00,  2.,  6., 1., &! ,'FARALLONDEPAJAROS 	    '&! 	 685	  
    19.6710,  145.4060,    857.00,  2.,  5., 1., &! ,'ASUNCION  		    '&! 	 686	  
    18.7700,  145.6700,    965.00,  2.,  1., 1., &! ,'AGRIGAN			    '&! 	 687	  
    18.1300,  145.8000,    570.00,  1.,  3., 1., &! ,'PAGAN			    '&! 	 688	  
    17.6000,  145.8300,    744.00,  6.,  5., 1., &! ,'ALAMAGAN  		    '&! 	 689	  
    16.8800,  145.8500,      0.00, -1.,  5., 1., &! ,'ZEALANDIABANK		    '&! 	 690	  
    16.7080,  145.7800,    538.00, -1.,  5., 1., &! ,'SARIGAN			    '&! 	 691	  
    17.3070,  145.8450,    287.00,  3.,  5., 1., &! ,'GUGUAN			    '&! 	 692	  
    15.9300,  145.6700,   -127.00, -1.,  5., 1., &! ,'EASTDIAMANTE		    '&! 	 693	  
    15.6200,  145.5700,   -230.00,  2.,  5., 1., &! ,'RUBY			    '&! 	 694	  
    16.3500,  145.6700,    790.00,  1.,  7., 1., &! ,'ANATAHAN  		    '&! 	 695	  
    14.6010,  144.7750,   -517.00,  1.,  7., 1., &! ,'NWROTA1			    '&! 	 696	  
    15.0000,  145.2500,    -43.00, -1.,  1., 1., &! ,'ESMERALDABANK		    '&! 	 697	  
    13.4000,  143.9200,      0.00, -1.,  1., 1., &! ,'FORECASTSEAMOUNT  	    '&! 	 698	  
    13.2500,  144.0200,  -1230.00, -1.,  1., 1., &! ,'SEAMOUNTX 		    '&! 	 699	  
    41.8020,  141.1700,    618.00,  3.,  5., 1., &! ,'ESAN			    '&! 	 700	  
    41.5070,  139.3710,    737.00,  4.,  1., 1., &! ,'OSHIMAOSHIMA		    '&! 	 701	  
    42.0610,  140.6810,   1131.00,  1.,  7., 1., &! ,'KOMAGATAKE		    '&! 	 702	  
    42.8800,  140.6300,   1154.00,  7.,  5., 1., &! ,'NISEKO			    '&! 	 703	  
    42.8300,  140.8150,   1898.00,  7.,  5., 1., &! ,'YOTEI			    '&! 	 704	  
    42.4890,  141.1630,    581.00,  3.,  5., 1., &! ,'KUTTARA			    '&! 	 705	  
    42.5410,  140.8430,    737.00,  1.,  5., 1., &! ,'USU			    '&! 	 706	  
    45.1800,  141.2500,   1721.00,  7.,  5., 1., &! ,'RISHIRI			    '&! 	 707	  
    42.6880,  141.3800,   1320.00,  2.,  5., 1., &! ,'SHIKOTSU  		    '&! 	 708	  
    43.4160,  142.6900,   2077.00,  1.,  5., 1., &! ,'TOKACHI			    '&! 	 709	  
    43.4530,  143.0360,   2013.00,  3.,  5., 1., &! ,'NIPESOTSUMARUYAMA 	    '&! 	 710	  
    43.3120,  143.0960,   1401.00, -2.,  5., 1., &! ,'SHIKARIBETSUGROUP 	    '&! 	 711	  
    43.6610,  142.8580,   2290.00,  4.,  5., 1., &! ,'DAISETSU  		    '&! 	 712	  
    43.3840,  144.0130,   1499.00,  1.,  6., 1., &! ,'AKAN			    '&! 	 713	  
    43.5700,  144.5650,    855.00,  6.,  5., 1., &! ,'MASHU			    '&! 	 714	  
    44.0730,  145.1260,   1660.00,  3.,  5., 1., &! ,'RAUSU			    '&! 	 715	  
    43.6080,  144.4430,    999.00,  6.,  5., 1., &! ,'KUTCHARO  		    '&! 	 716	  
    44.1310,  145.1650,   1563.00,  2.,  5., 1., &! ,'SHIRETOKOIWOZAN		    '&! 	 717	  
    43.8410,  145.5090,    543.00,  3.,  5., 1., &! ,'GOLOVNIN  		    '&! 	 718	  
    44.4200,  146.1350,   1189.00, -1.,  5., 1., &! ,'SMIRNOV			    '&! 	 719	  
    43.9760,  145.7360,    888.00,  3.,  5., 1., &! ,'MENDELEEV 		    '&! 	 720	  
    44.3510,  146.2560,   1819.00,  2.,  1., 1., &! ,'TIATIA			    '&! 	 721	  
    44.6080,  146.9940,    528.00,  7.,  5., 1., &! ,'LVINAYAPAST		    '&! 	 722	  
    44.4590,  146.9360,   1221.00, -1.,  5., 1., &! ,'BERUTARUBE		    '&! 	 723	  
    44.8050,  147.1350,   1206.00,  2.,  5., 1., &! ,'ATSONUPURI		    '&! 	 724	  
    44.8330,  147.3420,   1634.00, -1.,  5., 1., &! ,'BOGATYRRIDGE		    '&! 	 725	  
    45.0300,  147.2080,   -930.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	 726	  
    45.0260,  147.9220,   1211.00,  2.,  6., 1., &! ,'GROZNYGROUP		    '&! 	 727	  
    45.0970,  148.0240,   1132.00,  2.,  5., 1., &! ,'BARANSKY  		    '&! 	 728	  
    45.2500,  148.3500,    442.00, -2.,  5., 1., &! ,'GOLETSTORNYIGROUP 	    '&! 	 729	  
    45.3380,  147.9250,   1587.00,  3.,  1., 1., &! ,'CHIRIP			    '&! 	 730	  
    45.3870,  148.8430,   1125.00,  2.,  5., 1., &! ,'MEDVEZHIA 		    '&! 	 731	  
    45.5000,  148.8500,   1205.00, -1.,  5., 1., &! ,'DEMON			    '&! 	 732	  
    45.7700,  149.6800,   1426.00, -1.,  5., 1., &! ,'IVAOGROUP 		    '&! 	 733	  
    45.8800,  149.8300,    542.00, -2.,  5., 1., &! ,'RUDAKOV			    '&! 	 734	  
    45.9300,  149.9200,    998.00, -2.,  5., 1., &! ,'TRISESTRY 		    '&! 	 735	  
    46.0420,  150.0500,   1328.00,  2.,  5., 1., &! ,'KOLOKOLGROUP		    '&! 	 736	  
    46.1000,  150.5000,   -100.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	 737	  
    46.5250,  150.8750,    742.00,  2.,  5., 1., &! ,'CHIRPOI			    '&! 	 738	  
    46.4700,  151.2800,   -502.00,  2.,  5., 1., &! ,'UNNAMED			    '&! 	 739	  
    46.8200,  151.7800,   1540.00, -1.,  5., 1., &! ,'MILNE			    '&! 	 740	  
    46.8300,  151.7500,    891.00,  2.,  5., 1., &! ,'GORIASCHAIASOPKA  	    '&! 	 741	  
    46.9250,  151.9500,    624.00,  2.,  5., 1., &! ,'ZAVARITZKICALDERA 	    '&! 	 742	  
    47.1200,  152.2500,    678.00, -1.,  5., 1., &! ,'URATAMAN  		    '&! 	 743	  
    47.0200,  152.1200,   1360.00,  3.,  1., 1., &! ,'PREVOPEAK 		    '&! 	 744	  
    47.3500,  152.4750,   1172.00,  2.,  5., 1., &! ,'KETOI			    '&! 	 745	  
    47.6000,  152.9200,     36.00, -1.,  5., 1., &! ,'SREDNII			    '&! 	 746	  
    47.5200,  152.8000,    401.00,  3.,  5., 1., &! ,'USHISHUR  		    '&! 	 747	  
    47.7700,  153.0200,    956.00,  2.,  5., 1., &! ,'RASSHUA			    '&! 	 748	  
    48.0800,  153.3300,   -150.00,  2.,  5., 1., &! ,'UNNAMED			    '&! 	 749	  
    48.0920,  153.2000,   1496.00,  2.,  6., 1., &! ,'SARYCHEVPEAK		    '&! 	 750	  
    48.2920,  153.2500,    551.00,  2.,  1., 1., &! ,'RAIKOKE			    '&! 	 751	  
    48.9800,  153.4800,    724.00,  1.,  5., 1., &! ,'CHIRINKOTAN		    '&! 	 752	  
    48.9580,  153.9300,   1170.00,  2.,  5., 1., &! ,'EKARMA			    '&! 	 753	  
    48.8750,  154.1750,    934.00,  3.,  5., 1., &! ,'SINARKA			    '&! 	 754	  
    49.1200,  154.5080,   1145.00,  2.,  5., 1., &! ,'KHARIMKOTAN		    '&! 	 755	  
    49.3500,  154.7000,   1325.00,  2.,  5., 1., &! ,'TAORUSYRCALDERA		    '&! 	 756	  
    49.5700,  154.8080,   1018.00,  2.,  5., 1., &! ,'NEMOPEAK  		    '&! 	 757	  
    50.2000,  154.9800,    761.00, -1.,  5., 1., &! ,'SHIRINKI  		    '&! 	 758	  
    50.2700,  155.2500,   1772.00,  3.,  5., 1., &! ,'FUSSPEAK  		    '&! 	 759	  
    50.2500,  155.4300,   1681.00, -1.,  5., 1., &! ,'LOMONOSOVGROUP		    '&! 	 760	  
    50.1300,  155.3700,   1345.00,  2.,  5., 1., &! ,'KARPINSKYGROUP		    '&! 	 761	  
    50.3250,  155.4580,   1816.00,  1.,  3., 1., &! ,'CHIKURACHKI		    '&! 	 762	  
    50.5500,  155.9700,   1183.00, -1.,  5., 1., &! ,'VERNADSKIIRIDGE		    '&! 	 763	  
    50.6800,  156.0200,   1156.00,  1.,  6., 1., &! ,'EBEKO			    '&! 	 764	  
    50.8580,  155.5500,   2339.00,  2.,  2., 1., &! ,'ALAID			    '&! 	 765	  
    51.1000,  156.7200,    503.00, -1.,  1., 1., &! ,'MASHKOVTSEV		    '&! 	 766	  
    51.3000,  156.8700,   2156.00,  6.,  1., 1., &! ,'KAMBALNY  		    '&! 	 767	  
    51.5700,  156.6000,    705.00,  7.,  1., 1., &! ,'YAVINSKY  		    '&! 	 768	  
    51.4500,  156.9700,   1070.00,  6.,  5., 1., &! ,'DIKYGREBEN		    '&! 	 769	  
    51.4500,  157.1200,     81.00,  7.,  5., 1., &! ,'KURILELAKE		    '&! 	 770	  
    51.3570,  156.7500,   1812.00,  5.,  1., 1., &! ,'KOSHELEV  		    '&! 	 771	  
    51.4900,  157.2000,   1578.00,  2.,  5., 1., &! ,'ILYINSKY  		    '&! 	 772	  
    51.6500,  157.3500,    900.00, -1.,  1., 1., &! ,'KELL			    '&! 	 773	  
    51.7500,  157.2700,    892.00, -1.,  1., 1., &! ,'BELENKAYA 		    '&! 	 774	  
    51.5700,  157.3230,   1953.00,  2.,  1., 1., &! ,'ZHELTOVSKY		    '&! 	 775	  
    51.8800,  157.3800,    562.00, -1.,  1., 1., &! ,'OZERNOY			    '&! 	 776	  
    52.0200,  157.5300,    681.00, -1.,  1., 1., &! ,'OLKOVIYVOLCGROUP  	    '&! 	 777	  
    52.0630,  157.7030,   2090.00,  7.,  5., 1., &! ,'KHODUTKA  		    '&! 	 778	  
    52.1130,  157.8490,   1322.00, -1.,  5., 1., &! ,'PIRATKOVSKY		    '&! 	 779	  
    52.1460,  157.3220,    719.00, -1.,  1., 1., &! ,'OSTANETS  		    '&! 	 780	  
    52.2200,  157.4280,    791.00, -1.,  1., 1., &! ,'OTDELNIY  		    '&! 	 781	  
    52.2630,  157.7870,    858.00, -1.,  1., 1., &! ,'GOLAYA			    '&! 	 782	  
    52.3550,  157.8270,   1910.00, -1.,  1., 1., &! ,'ASACHA			    '&! 	 783	  
    52.4300,  157.9300,   1234.00, -1.,  1., 1., &! ,'VISOKIY			    '&! 	 784	  
    51.8000,  157.5300,   1079.00,  2.,  5., 1., &! ,'KSUDACH			    '&! 	 785	  
    52.4530,  158.1950,   2322.00,  1.,  2., 1., &! ,'MUTNOVSKY 		    '&! 	 786	  
    52.5580,  158.0300,   1829.00,  2.,  5., 1., &! ,'GORELY			    '&! 	 787	  
    52.5700,  157.0200,    610.00, -1.,  1., 1., &! ,'UNNAMED			    '&! 	 788	  
    52.6300,  157.5800,   1021.00,  6.,  1., 1., &! ,'TOLMACHEVDOL		    '&! 	 789	  
    52.7000,  158.2800,   2173.00,  7.,  5., 1., &! ,'VILYUCHIK 		    '&! 	 790	  
    52.8230,  158.2700,    870.00,  7.,  1., 1., &! ,'BARKHATNAYASOPKA  	    '&! 	 791	  
    52.9200,  158.5200,    450.00, -1.,  1., 1., &! ,'UNNAMED			    '&! 	 792	  
    52.8800,  158.3000,    700.00, -1.,  1., 1., &! ,'UNNAMED			    '&! 	 793	  
    52.9000,  157.7800,   1200.00, -1.,  1., 1., &! ,'BOLSHEBANNAYA		    '&! 	 794	  
    52.5430,  157.3350,   2475.00,  4.,  1., 1., &! ,'OPALA			    '&! 	 795	  
    53.3200,  158.6880,   3456.00,  1.,  5., 1., &! ,'KORYAKSKY 		    '&! 	 796	  
    53.2550,  158.8300,   2741.00,  1.,  5., 1., &! ,'AVACHINSKY		    '&! 	 797	  
    53.6370,  158.9220,   2285.00, -1.,  5., 1., &! ,'DZENZURSKY		    '&! 	 798	  
    53.7500,  158.4500,    520.00,  6.,  1., 1., &! ,'VEER			    '&! 	 799	  
    53.8300,  158.0500,   1150.00,  6.,  1., 1., &! ,'KOSTAKAN  		    '&! 	 800	  
    53.9050,  158.0700,   2278.00,  7.,  5., 1., &! ,'BAKENING  		    '&! 	 801	  
    53.9050,  158.3850,   1567.00,  7.,  1., 1., &! ,'ZAVARITSKY		    '&! 	 802	  
    53.9800,  159.4500,   1180.00,  2.,  5., 1., &! ,'AKADEMIANAUK		    '&! 	 803	  
    53.5900,  159.1470,   2958.00,  2.,  5., 1., &! ,'ZHUPANOVSKY		    '&! 	 804	  
    54.0500,  159.4500,   1536.00,  1.,  6., 1., &! ,'KARYMSKY  		    '&! 	 805	  
    54.1300,  159.6700,   1560.00,  2.,  1., 1., &! ,'MALYSEMIACHIK		    '&! 	 806	  
    54.3200,  160.0200,   1720.00,  7.,  1., 1., &! ,'BOLSHOISEMIACHIK  	    '&! 	 807	  
    54.5300,  159.8000,   2353.00,  7.,  5., 1., &! ,'TAUNSHITS 		    '&! 	 808	  
    54.5000,  159.9700,   1617.00,  7.,  5., 1., &! ,'UZON			    '&! 	 809	  
    54.4870,  160.2530,   1552.00,  5.,  1., 1., &! ,'KIKHPINYCH		    '&! 	 810	  
    54.5930,  160.2730,   1856.00,  5.,  1., 1., &! ,'KRASHENINNIKOV		    '&! 	 811	  
    54.9200,  160.6300,   2020.00, -1.,  1., 1., &! ,'SCHMIDT			    '&! 	 812	  
    54.7530,  160.5270,   3528.00,  2.,  1., 1., &! ,'KRONOTSKY 		    '&! 	 813	  
    54.9730,  160.7020,   2576.00,  7.,  1., 1., &! ,'GAMCHEN			    '&! 	 814	  
    55.0700,  160.7700,   2161.00,  7.,  5., 1., &! ,'VYSOKY			    '&! 	 815	  
    55.0320,  160.7200,   2070.00,  6.,  1., 1., &! ,'KOMAROV			    '&! 	 816	  
    55.9200,  161.7500,      0.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	 817	  
    55.1300,  160.3200,   2376.00,  2.,  5., 1., &! ,'KIZIMEN			    '&! 	 818	  
    55.7550,  160.5270,   2923.00, -1.,  5., 1., &! ,'UDINA			    '&! 	 819	  
    55.8620,  160.6030,   3081.00, -1.,  5., 1., &! ,'ZIMINA			    '&! 	 820	  
    55.8300,  160.3300,   3682.00,  2.,  2., 1., &! ,'TOLBACHIK 		    '&! 	 821	  
    56.0200,  160.5930,   4585.00, -1.,  1., 1., &! ,'KAMEN			    '&! 	 822	  
    55.9780,  160.5870,   2882.00,  1.,  7., 1., &! ,'BEZYMIANNY		    '&! 	 823	  
    56.0700,  160.4700,   3943.00,  3.,  1., 1., &! ,'USHKOVSKY 		    '&! 	 824	  
    56.0570,  160.6380,   4835.00,  1.,  2., 1., &! ,'KLIUCHEVSKOI		    '&! 	 825	  
    55.4200,  167.3300,   -300.00,  7.,  2., 1., &! ,'PIIP			    '&! 	 826	  
    54.7500,  157.3800,   2000.00,  5.,  5., 1., &! ,'KHANGAR			    '&! 	 827	  
    55.5500,  157.4700,   1868.00,  7.,  5., 1., &! ,'CHERPUKGROUP		    '&! 	 828	  
    56.6530,  161.3600,   3283.00,  1.,  6., 1., &! ,'SHIVELUCH 		    '&! 	 829	  
    55.6800,  157.7300,   3621.00,  4.,  1., 1., &! ,'ICHINSKY  		    '&! 	 830	  
    55.8200,  157.9800,   1802.00, -2.,  1., 1., &! ,'MALYPAYALPAN		    '&! 	 831	  
    55.8800,  157.7800,   1906.00, -2.,  1., 1., &! ,'BOLSHOIPAYALPAN		    '&! 	 832	  
    55.2000,  158.4700,   1236.00, -2.,  1., 1., &! ,'PLOSKY			    '&! 	 833	  
    55.4300,  158.6500,   1956.00, -2.,  1., 1., &! ,'AKHTANG			    '&! 	 834	  
    55.5800,  158.3800,   2016.00, -2.,  1., 1., &! ,'KOZYREVSKY		    '&! 	 835	  
    55.6500,  158.8000,   1442.00, -2.,  1., 1., &! ,'ROMANOVKA 		    '&! 	 836	  
    56.0800,  158.3800,   1692.00, -2.,  1., 1., &! ,'UKSICHAN  		    '&! 	 837	  
    56.4700,  157.8000,   1401.00,  7.,  1., 1., &! ,'BOLSHOIKEKUKNAYSKY	    '&! 	 838	  
    56.3700,  158.3700,    915.00, -2.,  1., 1., &! ,'KULKEV			    '&! 	 839	  
    56.3300,  158.6700,   1170.00, -2.,  1., 1., &! ,'GEODESISTOY		    '&! 	 840	  
    56.3200,  158.8300,   1828.00, -2.,  1., 1., &! ,'ANAUN			    '&! 	 841	  
    56.3700,  159.0300,   1554.00, -2.,  1., 1., &! ,'KRAINY			    '&! 	 842	  
    56.4000,  158.8500,   1377.00, -2.,  1., 1., &! ,'KEKURNY			    '&! 	 843	  
    56.5700,  158.5200,   1046.00, -2.,  1., 1., &! ,'EGGELLA			    '&! 	 844	  
    56.8200,  158.9500,   1185.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	 845	  
    56.5200,  159.5300,   1400.00, -2.,  1., 1., &! ,'VERKHOVOY 		    '&! 	 846	  
    56.7000,  159.6500,   2598.00,  5.,  1., 1., &! ,'ALNEYCHASHAKONDZHA	    '&! 	 847	  
    56.8200,  159.6700,   1778.00, -2.,  1., 1., &! ,'CHERNY			    '&! 	 848	  
    56.8500,  159.8000,   1427.00, -2.,  1., 1., &! ,'POGRANYCHNY		    '&! 	 849	  
    56.8800,  159.9500,   1349.00, -2.,  1., 1., &! ,'ZAOZERNY  		    '&! 	 850	  
    56.9700,  159.7800,   1244.00, -2.,  1., 1., &! ,'BLIZNETS  		    '&! 	 851	  
    57.1000,  159.9300,   1527.00, -2.,  1., 1., &! ,'KEBENEY			    '&! 	 852	  
    57.1300,  160.4000,    965.00, -2.,  1., 1., &! ,'FEDOTYCH  		    '&! 	 853	  
    57.1500,  161.0800,    379.00,  7.,  5., 1., &! ,'SHISHEIKA 		    '&! 	 854	  
    57.2000,  159.8300,    765.00,  7.,  1., 1., &! ,'TERPUK			    '&! 	 855	  
    57.2700,  160.0800,   1241.00,  7.,  1., 1., &! ,'SEDANKINSKY		    '&! 	 856	  
    57.3000,  159.8300,   1333.00, -2.,  1., 1., &! ,'LEUTONGEY 		    '&! 	 857	  
    57.3200,  159.9700,   1533.00, -2.,  1., 1., &! ,'TUZOVSKY  		    '&! 	 858	  
    57.3300,  160.2000,   2125.00,  6.,  5., 1., &! ,'GORNYINSTITUTE		    '&! 	 859	  
    57.3500,  160.9700,    583.00,  6.,  1., 1., &! ,'KINENIN			    '&! 	 860	  
    57.3500,  161.3700,    265.00,  7.,  5., 1., &! ,'BLIZNETSY 		    '&! 	 861	  
    57.4000,  160.1000,   1559.00,  7.,  1., 1., &! ,'TITILA			    '&! 	 862	  
    57.4700,  160.2500,   1641.00, -2.,  1., 1., &! ,'MEZHDUSOPOCHNY		    '&! 	 863	  
    57.4500,  160.3700,   2525.00, -2.,  1., 1., &! ,'SHISHEL			    '&! 	 864	  
    57.5500,  160.5300,   1381.00,  7.,  1., 1., &! ,'ELOVSKY			    '&! 	 865	  
    57.7000,  160.4000,   1853.00, -2.,  1., 1., &! ,'ALNGEY			    '&! 	 866	  
    57.7000,  160.5800,   1643.00, -2.,  1., 1., &! ,'UKA			    '&! 	 867	  
    57.8000,  160.6700,   1582.00, -2.,  1., 1., &! ,'KAILENEY  		    '&! 	 868	  
    57.8300,  160.2500,   1255.00, -2.,  1., 1., &! ,'PLOSKY			    '&! 	 869	  
    57.8800,  160.5300,   2080.00, -2.,  1., 1., &! ,'BELY			    '&! 	 870	  
    57.9700,  160.6500,   1764.00,  7.,  1., 1., &! ,'NYLGIMELKIN		    '&! 	 871	  
    58.0200,  160.8000,   2169.00, -2.,  1., 1., &! ,'SNEZHNIY  		    '&! 	 872	  
    58.0800,  160.7700,   2300.00, -2.,  1., 1., &! ,'IKTUNUP			    '&! 	 873	  
    58.1300,  160.8200,   2171.00,  7.,  5., 1., &! ,'SPOKOINY  		    '&! 	 874	  
    58.1800,  160.8200,   2552.00,  7.,  1., 1., &! ,'OSTRY			    '&! 	 875	  
    58.2000,  160.9700,   2169.00, -2.,  1., 1., &! ,'SNEGOVOY  		    '&! 	 876	  
    58.2800,  160.8700,   1936.00,  7.,  1., 1., &! ,'SEVERNY			    '&! 	 877	  
    58.4000,  161.0800,   1340.00, -2.,  1., 1., &! ,'IETTUNUP  		    '&! 	 878	  
    58.3700,  160.6200,   1225.00, -2.,  1., 1., &! ,'VOYAMPOLSKY		    '&! 	 879	  
    47.0000,  137.5000,      0.00, -1.,  1., 1., &! ,'SIKHOTEALIN		    '&! 	 880	  
    56.2800,  117.7700,   2180.00,  7.,  1., 1., &! ,'UDOKANPLATEAU		    '&! 	 881	  
    53.7000,  113.3000,   1250.00, -1.,  1., 1., &! ,'VITIMPLATEAU		    '&! 	 882	  
    51.5000,  102.5000,   1200.00, -2.,  1., 1., &! ,'TUNKINDEPRESSION  	    '&! 	 883	  
    52.7000,   98.9800,   2077.00, -1.,  1., 1., &! ,'OKAPLATEAU		    '&! 	 884	  
    52.5200,   98.6000,   2765.00, -1.,  1., 1., &! ,'AZASPLATEAU		    '&! 	 885	  
    48.1700,   99.7000,   2400.00,  7.,  1., 1., &! ,'TARYATUCHULUTU		    '&! 	 886	  
    48.6700,  102.7500,   1886.00, -1.,  5., 1., &! ,'KHANUYGOL 		    '&! 	 887	  
    47.1200,  109.0800,   1162.00, -2.,  1., 1., &! ,'BUSOBO			    '&! 	 888	  
    45.3300,  114.0000,   1778.00, -1.,  1., 1., &! ,'DARIGANGAVOLCFIELD	    '&! 	 889	  
    45.2800,  106.7000,   1120.00, -2.,  1., 1., &! ,'MIDDLEGOBI		    '&! 	 890	  
    42.9000,   89.2500,      0.00,  6.,  5., 1., &! ,'TURFAN			    '&! 	 891	  
    42.5000,   82.5000,      0.00,  6.,  5., 1., &! ,'TIANSHANVOLCGROUP 	    '&! 	 892	  
    35.5200,   80.2000,   5808.00,  2.,  5., 1., &! ,'KUNLUNVOLCGROUP		    '&! 	 893	  
    35.8500,   91.7000,   5400.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	 894	  
    41.4700,  113.0000,   1700.00, -2.,  1., 1., &! ,'HONGGEERTU		    '&! 	 895	  
    47.5000,  120.7000,      0.00, -1.,  1., 1., &! ,'ARSHAN			    '&! 	 896	  
    49.3700,  125.9200,    670.00, -1.,  1., 1., &! ,'KELUOGROUP		    '&! 	 897	  
    48.7200,  126.1200,    597.00,  4.,  1., 1., &! ,'WUDALIANCHI		    '&! 	 898	  
    44.0800,  128.8300,   1000.00,  7.,  1., 1., &! ,'JINGBO			    '&! 	 899	  
    42.3300,  126.5000,   1000.00,  6.,  1., 1., &! ,'LONGGANGGROUP		    '&! 	 900	  
    41.9800,  128.0800,   2744.00,  2.,  5., 1., &! ,'CHANGBAISHAN		    '&! 	 901	  
    41.3300,  128.0000,      0.00, -2.,  5., 1., &! ,'XIANJINDAO		    '&! 	 902	  
    38.3300,  127.3300,    452.00, -2.,  1., 1., &! ,'CHUGARYONG		    '&! 	 903	  
    37.5000,  130.8700,    984.00,  7.,  5., 1., &! ,'ULREUNG			    '&! 	 904	  
    33.3700,  126.5300,   1950.00,  6.,  1., 1., &! ,'HALLA			    '&! 	 905	  
    52.3500,  175.9110,    656.00, -2.,  5., 1., &! ,'BULDIR			    '&! 	 906	  
    52.1030,  177.6020,   1220.00,  2.,  1., 1., &! ,'KISKA			    '&! 	 907	  
    52.0150,  178.1360,   1160.00, -1.,  5., 1., &! ,'SEGULA			    '&! 	 908	  
    51.9700,  178.3300,    328.00, -2.,  1., 1., &! ,'DAVIDOF			    '&! 	 909	  
    51.9500,  178.5430,   1174.00,  3.,  5., 1., &! ,'LITTLESITKIN		    '&! 	 910	  
    51.9300,  179.5800,   1221.00,  2.,  5., 1., &! ,'SEMISOPOCHNOI		    '&! 	 911	  
    51.7900, -178.7940,   1573.00,  2.,  5., 1., &! ,'GARELOI			    '&! 	 912	  
    51.8850, -178.1460,   1806.00,  2.,  5., 1., &! ,'TANAGA			    '&! 	 913	  
    51.8730, -178.0060,   1449.00,  5.,  5., 1., &! ,'TAKAWANGHA		    '&! 	 914	  
    51.9100, -177.4380,    738.00, -2.,  5., 1., &! ,'BOBROF			    '&! 	 915	  
    51.9230, -177.1680,   1307.00,  2.,  1., 1., &! ,'KANAGA			    '&! 	 916	  
    51.9440, -176.7470,   1196.00, -2.,  5., 1., &! ,'MOFFETT			    '&! 	 917	  
    52.0760, -176.1300,   1740.00,  2.,  6., 1., &! ,'GREATSITKIN		    '&! 	 918	  
    52.1770, -175.5080,    314.00,  1.,  7., 1., &! ,'KASATOCHI 		    '&! 	 919	  
    52.2200, -175.1300,    273.00, -2.,  5., 1., &! ,'KONIUJI			    '&! 	 920	  
    52.0500, -174.9500,    560.00, -2.,  5., 1., &! ,'SERGIEF			    '&! 	 921	  
    52.3320, -174.1370,   1451.00,  2.,  6., 1., &! ,'ATKA			    '&! 	 922	  
    52.3810, -174.1540,   1533.00,  1.,  6., 1., &! ,'KOROVIN			    '&! 	 923	  
    52.3150, -172.5100,   1054.00,  2.,  5., 1., &! ,'SEGUAM			    '&! 	 924	  
    52.5000, -171.2520,   1066.00,  2.,  6., 1., &! ,'AMUKTA			    '&! 	 925	  
    52.5770, -171.1300,   1142.00, -1.,  5., 1., &! ,'CHAGULAK  		    '&! 	 926	  
    52.6430, -170.6290,    550.00,  2.,  7., 1., &! ,'YUNASKA			    '&! 	 927	  
    52.7420, -170.1110,   1280.00, -1.,  5., 1., &! ,'HERBERT			    '&! 	 928	  
    52.8940, -170.0540,   1620.00,  3.,  6., 1., &! ,'CARLISLE  		    '&! 	 929	  
    52.8250, -169.9440,   1730.00,  1.,  7., 1., &! ,'CLEVELAND 		    '&! 	 930	  
    52.8300, -169.7700,   1170.00, -1.,  5., 1., &! ,'TANA			    '&! 	 931	  
    53.0650, -169.7700,    888.00, -1.,  5., 1., &! ,'ULIAGA			    '&! 	 932	  
    52.9740, -169.7200,    893.00,  2.,  5., 1., &! ,'KAGAMIL			    '&! 	 933	  
    53.1300, -168.6930,   2149.00,  3.,  5., 1., &! ,'VSEVIDOF  		    '&! 	 934	  
    53.1570, -168.5390,   1984.00, -1.,  5., 1., &! ,'RECHESCHNOI		    '&! 	 935	  
    53.4300, -168.1300,   1073.00,  1.,  7., 1., &! ,'OKMOK			    '&! 	 936	  
    53.9300, -168.0300,    150.00,  2.,  7., 1., &! ,'BOGOSLOF  		    '&! 	 937	  
    53.8910, -166.9230,   1800.00,  2.,  6., 1., &! ,'MAKUSHIN  		    '&! 	 938	  
    54.1340, -165.9860,   1303.00,  2.,  6., 1., &! ,'AKUTAN			    '&! 	 939	  
    54.5180, -164.6500,   1654.00,  2.,  7., 1., &! ,'WESTDAHL  		    '&! 	 940	  
    54.6500, -164.4300,   1112.00,  3.,  5., 1., &! ,'FISHER			    '&! 	 941	  
    54.7560, -163.9700,   2857.00,  1.,  7., 1., &! ,'SHISHALDIN		    '&! 	 942	  
    54.7650, -163.7230,   2446.00, -2.,  5., 1., &! ,'ISANOTSKI 		    '&! 	 943	  
    54.8000, -163.5890,   1871.00,  7.,  5., 1., &! ,'ROUNDTOP  		    '&! 	 944	  
    55.4240, -163.1490,    488.00,  4.,  5., 1., &! ,'AMAK			    '&! 	 945	  
    55.0820, -162.8140,   2012.00, -1.,  5., 1., &! ,'FROSTY			    '&! 	 946	  
    55.1680, -162.2720,   1506.00, -1.,  5., 1., &! ,'DUTTON			    '&! 	 947	  
    55.3410, -162.0790,   1436.00, -1.,  5., 1., &! ,'EMMONSLAKE		    '&! 	 948	  
    55.4200, -161.8870,   2519.00,  1.,  7., 1., &! ,'PAVLOF			    '&! 	 949	  
    55.4530, -161.8430,   2142.00, -1.,  5., 1., &! ,'PAVLOFSISTER		    '&! 	 950	  
    55.6410, -161.2140,   1354.00,  7.,  5., 1., &! ,'DANA			    '&! 	 951	  
    55.9130, -160.0410,   1323.00, -1.,  5., 1., &! ,'STEPOVAKBAY2		    '&! 	 952	  
    55.9290, -160.0020,   1555.00, -2.,  5., 1., &! ,'STEPOVAKBAY3		    '&! 	 953	  
    55.9540, -159.9540,   1557.00, -1.,  5., 1., &! ,'STEPOVAKBAY4		    '&! 	 954	  
    56.0110, -159.7970,   1895.00,  2.,  5., 1., &! ,'KUPREANOF 		    '&! 	 955	  
    56.1700, -159.3800,   2507.00,  1.,  6., 1., &! ,'VENIAMINOF		    '&! 	 956	  
    56.5520, -158.7850,   1032.00,  7.,  5., 1., &! ,'BLACKPEAK 		    '&! 	 957	  
    56.8800, -158.1700,   1341.00,  2.,  6., 1., &! ,'ANIAKCHAK 		    '&! 	 958	  
    57.0190, -157.1850,   1345.00,  7.,  5., 1., &! ,'YANTARNI  		    '&! 	 959	  
    57.1350, -156.9900,   2221.00,  2.,  5., 1., &! ,'CHIGINAGAK		    '&! 	 960	  
    57.2030, -156.7450,   1677.00, -1.,  5., 1., &! ,'KIALAGVIK 		    '&! 	 961	  
    57.7510, -156.3680,   1474.00,  3.,  5., 1., &! ,'UGASHIKPEULIK		    '&! 	 962	  
    57.8320, -156.5100,     91.00,  2.,  6., 1., &! ,'UKINREKMAARS		    '&! 	 963	  
    57.8700, -155.4200,    300.00, -1.,  5., 1., &! ,'UNNAMED			    '&! 	 964	  
    58.1720, -155.3610,   1863.00,  2.,  5., 1., &! ,'MARTIN			    '&! 	 965	  
    58.1950, -155.2530,   2165.00,  7.,  5., 1., &! ,'MAGEIK			    '&! 	 966	  
    58.2360, -155.1000,   1864.00,  2.,  7., 1., &! ,'TRIDENT			    '&! 	 967	  
    58.2800, -154.9630,   2047.00,  2.,  5., 1., &! ,'KATMAI			    '&! 	 968	  
    58.2700, -155.1570,    841.00,  2.,  5., 1., &! ,'NOVARUPTA 		    '&! 	 969	  
    58.3540, -155.0920,   2317.00,  7.,  5., 1., &! ,'GRIGGS			    '&! 	 970	  
    58.3360, -154.6820,   2162.00,  4.,  5., 1., &! ,'SNOWYMOUNTAIN		    '&! 	 971	  
    58.4180, -154.4490,   2287.00, -2.,  5., 1., &! ,'DENISON			    '&! 	 972	  
    58.4300, -154.4000,   2272.00, -1.,  5., 1., &! ,'STELLER			    '&! 	 973	  
    58.4530, -154.3550,   2043.00, -1.,  5., 1., &! ,'KUKAK			    '&! 	 974	  
    58.6080, -154.0280,    901.00,  7.,  5., 1., &! ,'KAGUYAK			    '&! 	 975	  
    58.7700, -153.6720,   2105.00,  1.,  6., 1., &! ,'FOURPEAKED		    '&! 	 976	  
    58.8550, -153.5420,   2140.00, -1.,  5., 1., &! ,'DOUGLAS			    '&! 	 977	  
    59.3630, -153.4300,   1252.00,  1.,  7., 1., &! ,'AUGUSTINE 		    '&! 	 978	  
    60.0320, -153.0900,   3053.00,  3.,  5., 1., &! ,'ILIAMNA			    '&! 	 979	  
    60.4850, -152.7420,   3108.00,  2.,  7., 1., &! ,'REDOUBT			    '&! 	 980	  
    61.2990, -152.2510,   3374.00,  2.,  7., 1., &! ,'SPURR			    '&! 	 981	  
    61.6400, -152.4110,   3034.00,  6.,  5., 1., &! ,'HAYES			    '&! 	 982	  
    57.1800, -170.3000,    203.00,  7.,  1., 1., &! ,'STPAULISLAND		    '&! 	 983	  
    60.0200, -166.3300,    511.00, -1.,  1., 1., &! ,'NUNIVAKISLAND		    '&! 	 984	  
    61.4300, -164.4700,    190.00, -1.,  1., 1., &! ,'INGAKSLUGWATHILLS 	    '&! 	 985	  
    63.4500, -162.1200,    715.00, -1.,  1., 1., &! ,'ST.MICHAEL		    '&! 	 986	  
    63.6000, -170.4300,    673.00, -1.,  1., 1., &! ,'KOOKOOLIGITMOUNTAINS	    '&! 	 987	  
    65.6000, -163.9200,    610.00,  6.,  1., 1., &! ,'IMURUKLAKE		    '&! 	 988	  
    64.0700, -148.4200,    830.00,  7.,  1., 1., &! ,'BUZZARDCREEK		    '&! 	 989	  
    62.2200, -144.1300,   4949.00, -2.,  5., 1., &! ,'SANFORD			    '&! 	 990	  
    62.0000, -144.0200,   4317.00,  1.,  5., 1., &! ,'WRANGELL  		    '&! 	 991	  
    62.1300, -143.0800,   2755.00, -2.,  1., 1., &! ,'GORDON			    '&! 	 992	  
    61.3800, -141.7500,   5005.00,  6.,  5., 1., &! ,'CHURCHILL 		    '&! 	 993	  
    57.0500, -135.7500,    970.00,  7.,  5., 1., &! ,'EDGECUMBE 		    '&! 	 994	  
    56.5000, -133.1000,     15.00, -1.,  1., 1., &! ,'DUNCANCANAL		    '&! 	 995	  
    55.2500, -133.3000,     50.00, -1.,  1., 1., &! ,'TLEVAKSTRAITSUEMEZIS.	    '&! 	 996	  
    55.3200, -131.0500,    500.00, -2.,  1., 1., &! ,'BEHMCANALRUDYERDBAY	    '&! 	 997	  
    62.9300, -137.3800,   1239.00, -2.,  1., 1., &! ,'FORTSELKIRK		    '&! 	 998	  
    60.4200, -135.4200,   2217.00, -1.,  1., 1., &! ,'ALLIGATORLAKE		    '&! 	 999	  
    59.6800, -133.3200,   1880.00, -1.,  1., 1., &! ,'ATLINVOLCFIELD		    '&! 	1000	  
    59.3700, -130.5800,   2123.00, -1.,  1., 1., &! ,'TUYAVOLCFIELD		    '&! 	1001	  
    58.6000, -131.9700,   2012.00, -2.,  5., 1., &! ,'HEARTPEAKS		    '&! 	1002	  
    58.4200, -131.3500,   2190.00, -2.,  5., 1., &! ,'LEVELMOUNTAIN		    '&! 	1003	  
    57.7200, -130.6300,   2786.00,  6.,  1., 1., &! ,'EDZIZA			    '&! 	1004	  
    57.4300, -130.6800,   2430.00, -1.,  5., 1., &! ,'SPECTRUMRANGE		    '&! 	1005	  
    56.7800, -131.2800,   1850.00,  7.,  5., 1., &! ,'HOODOOMOUNTAIN		    '&! 	1006	  
    56.5800, -130.5500,   1880.00,  3.,  1., 1., &! ,'ISKUTUNUKRIVERCONES	    '&! 	1007	  
    55.1200, -128.9000,    609.00,  4.,  1., 1., &! ,'TSEAXRIVERCONE		    '&! 	1008	  
    54.7000, -130.2300,    335.00, -1.,  1., 1., &! ,'CROWLAGOON		    '&! 	1009	  
    52.5000, -128.7300,    335.00, -1.,  1., 1., &! ,'MILBANKESOUNDGROUP	    '&! 	1010	  
    52.4700, -124.7000,   1921.00, -1.,  1., 1., &! ,'SATAHMOUNTAIN		    '&! 	1011	  
    52.9000, -123.7300,   1230.00,  7.,  1., 1., &! ,'NAZKO			    '&! 	1012	  
    52.3300, -120.5700,   2015.00,  5.,  1., 1., &! ,'WELLSGRAYCLEARWATER	    '&! 	1013	  
    51.4300, -126.3000,   3160.00, -1.,  5., 1., &! ,'SILVERTHRONE		    '&! 	1014	  
    50.8000, -123.4000,   2500.00, -1.,  1., 1., &! ,'BRIDGERIVERCONES  	    '&! 	1015	  
    50.6300, -123.5000,   2680.00,  7.,  5., 1., &! ,'MEAGER			    '&! 	1016	  
    49.9200, -123.0300,   2316.00, -1.,  5., 1., &! ,'GARIBALDILAKE		    '&! 	1017	  
    49.8500, -123.0000,   2678.00,  7.,  5., 1., &! ,'GARIBALDI 		    '&! 	1018	  
    48.7770, -121.8130,   3285.00,  3.,  6., 1., &! ,'BAKER			    '&! 	1019	  
    48.1120, -121.1130,   3213.00,  4.,  5., 1., &! ,'GLACIERPEAK		    '&! 	1020	  
    46.8530, -121.7600,   4392.00,  3.,  6., 1., &! ,'RAINIER			    '&! 	1021	  
    46.2060, -121.4900,   3742.00,  6.,  5., 1., &! ,'ADAMS			    '&! 	1022	  
    46.2000, -122.1800,   2549.00,  1.,  7., 1., &! ,'ST.HELENS 		    '&! 	1023	  
    45.8800, -122.0800,   1329.00,  7.,  1., 1., &! ,'WESTCRATER		    '&! 	1024	  
    45.9300, -121.8200,   1806.00,  7.,  1., 1., &! ,'INDIANHEAVEN		    '&! 	1025	  
    45.3740, -121.6950,   3426.00,  3.,  5., 1., &! ,'HOOD			    '&! 	1026	  
    44.6740, -121.8000,   3199.00,  6.,  5., 1., &! ,'JEFFERSON 		    '&! 	1027	  
    44.4110, -121.7740,   1230.00,  6.,  1., 1., &! ,'BLUELAKECRATER		    '&! 	1028	  
    44.3800, -121.9300,   1664.00,  6.,  1., 1., &! ,'SANDMOUNTAINFIELD 	    '&! 	1029	  
    44.2850, -121.8410,   2095.00,  6.,  1., 1., &! ,'BELKNAP			    '&! 	1030	  
    44.1700, -121.7700,   3074.00,  6.,  1., 1., &! ,'NORTHSISTERFIELD  	    '&! 	1031	  
    44.1030, -121.7680,   3157.00,  7.,  5., 1., &! ,'SOUTHSISTER		    '&! 	1032	  
    43.9790, -121.6880,   2763.00,  7.,  5., 1., &! ,'BACHELOR  		    '&! 	1033	  
    43.5700, -121.8200,   2163.00,  7.,  1., 1., &! ,'DAVISLAKE 		    '&! 	1034	  
    43.7220, -121.2290,   2434.00,  6.,  5., 1., &! ,'NEWBERRY  		    '&! 	1035	  
    43.5120, -120.8610,   1698.00, -2.,  1., 1., &! ,'DEVILSGARDEN		    '&! 	1036	  
    43.4720, -120.7540,   1711.00, -2.,  1., 1., &! ,'SQUAWRIDGELAVAFIELD	    '&! 	1037	  
    43.3610, -120.6690,   1501.00, -2.,  1., 1., &! ,'FOURCRATERSLAVAFIELD	    '&! 	1038	  
    43.2410, -122.1080,   1956.00, -2.,  1., 1., &! ,'CINNAMONBUTTE		    '&! 	1039	  
    42.9300, -122.1200,   2487.00,  7.,  5., 1., &! ,'CRATERLAKE		    '&! 	1040	  
    43.1000, -118.7500,   1435.00, -2.,  1., 1., &! ,'DIAMONDCRATERS		    '&! 	1041	  
    43.1470, -117.4600,   1473.00,  7.,  1., 1., &! ,'JORDANCRATERS		    '&! 	1042	  
    41.4090, -122.1930,   4317.00,  4.,  7., 1., &! ,'SHASTA			    '&! 	1043	  
    41.6110, -121.5540,   2412.00,  6.,  5., 1., &! ,'MEDICINELAKE		    '&! 	1044	  
    41.1780, -121.4430,   1174.00, -2.,  1., 1., &! ,'BRUSHYBUTTE		    '&! 	1045	  
    40.7770, -121.5910,   1631.00, -2.,  1., 1., &! ,'TWINBUTTES		    '&! 	1046	  
    40.7310, -121.8410,   1535.00, -2.,  1., 1., &! ,'SILVERLAKE		    '&! 	1047	  
    40.6800, -121.5500,   2191.00, -2.,  1., 1., &! ,'TUMBLEBUTTES		    '&! 	1048	  
    40.4920, -121.5080,   3187.00,  2.,  7., 1., &! ,'LASSENVOLCCENTER  	    '&! 	1049	  
    40.6300, -120.8300,   1652.00, -2.,  1., 1., &! ,'EAGLELAKEFIELD		    '&! 	1050	  
    38.9700, -122.7700,   1439.00, -1.,  5., 1., &! ,'CLEARLAKE 		    '&! 	1051	  
    38.0000, -119.0300,   2121.00,  4.,  5., 1., &! ,'MONOLAKEVOLCFIELD 	    '&! 	1052	  
    37.8800, -119.0000,   2796.00,  6.,  5., 1., &! ,'MONOCRATERS		    '&! 	1053	  
    37.6920, -119.0200,   2629.00,  6.,  5., 1., &! ,'INYOCRATERS		    '&! 	1054	  
    37.6310, -119.0320,   3369.00,  6.,  5., 1., &! ,'MAMMOTHMOUNTAIN		    '&! 	1055	  
    37.0200, -117.4500,    752.00,  7.,  1., 1., &! ,'UBEHEBECRATERS		    '&! 	1056	  
    36.3580, -118.3200,   2886.00,  7.,  1., 1., &! ,'GOLDENTROUTCREEK  	    '&! 	1057	  
    36.0300, -117.8200,   2400.00, -2.,  5., 1., &! ,'COSOVOLCFIELD		    '&! 	1058	  
    34.7500, -116.6250,   1495.00, -2.,  1., 1., &! ,'LAVICLAKE 		    '&! 	1059	  
    43.1800, -114.3500,   1478.00,  7.,  1., 1., &! ,'SHOSHONELAVAFIELD 	    '&! 	1060	  
    43.4200, -113.5000,   2005.00,  7.,  1., 1., &! ,'CRATERSOFTHEMOON  	    '&! 	1061	  
    42.8800, -113.2200,   1604.00,  7.,  1., 1., &! ,'WAPILAVAFIELD		    '&! 	1062	  
    43.5000, -112.4500,   1631.00,  7.,  1., 1., &! ,'HELLSHALFACRE		    '&! 	1063	  
    44.4300, -110.6700,   2805.00,  7.,  5., 1., &! ,'YELLOWSTONE		    '&! 	1064	  
    39.5300, -118.8700,   1251.00, -1.,  1., 1., &! ,'SODALAKES 		    '&! 	1065	  
    37.2570, -113.6250,   1465.00, -2.,  1., 1., &! ,'SANTACLARA		    '&! 	1066	  
    37.3280, -112.4080,   2135.00, -1.,  1., 1., &! ,'BALDKNOLL 		    '&! 	1067	  
    37.5800, -112.6700,   2840.00,  6.,  1., 1., &! ,'MARKAGUNTPLATEAU  	    '&! 	1068	  
    38.9700, -112.5000,   1800.00,  6.,  1., 1., &! ,'BLACKROCKDESERT		    '&! 	1069	  
    39.6610, -107.0350,   2230.00,  7.,  1., 1., &! ,'DOTSERO			    '&! 	1070	  
    36.3800, -113.1300,   1555.00,  6.,  1., 1., &! ,'UINKARETFIELD		    '&! 	1071	  
    35.3700, -111.5000,   2447.00,  6.,  1., 1., &! ,'SUNSETCRATER		    '&! 	1072	  
    33.7800, -105.9300,   1731.00,  7.,  1., 1., &! ,'CARRIZOZO 		    '&! 	1073	  
    34.8000, -108.0000,   2550.00,  7.,  1., 1., &! ,'ZUNIBANDERA		    '&! 	1074	  
    47.9500, -129.1000,  -2050.00,  7.,  1., 1., &! ,'ENDEAVOURRIDGE		    '&! 	1075	  
    46.8800, -129.3300,  -2100.00,  7.,  1., 1., &! ,'COBBSEGMENT		    '&! 	1076	  
    46.5200, -129.5800,  -2400.00,  2.,  1., 1., &! ,'COAXIALSEGMENT		    '&! 	1077	  
    45.9500, -130.0000,  -1410.00,  2.,  1., 1., &! ,'AXIALSEAMOUNT		    '&! 	1078	  
    44.8300, -130.3000,  -2140.00,  2.,  1., 1., &! ,'CLEFTSEGMENT		    '&! 	1079	  
    42.6700, -126.7800,  -3000.00,  2.,  1., 1., &! ,'NORTHGORDARIDGE		    '&! 	1080	  
    40.9800, -127.5000,  -1700.00,  7.,  1., 1., &! ,'ESCANABASEGMENT		    '&! 	1081	  
    31.7500, -124.2500,  -2533.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	1082	  
    18.9200, -155.2700,   -975.00,  2.,  1., 1., &! ,'LOIHI			    '&! 	1083	  
    19.4210, -155.2870,   1222.00,  1.,  2., 1., &! ,'KILAUEA			    '&! 	1084	  
    19.4750, -155.6080,   4170.00,  2.,  2., 1., &! ,'MAUNALOA  		    '&! 	1085	  
    19.8200, -155.4700,   4205.00,  7.,  1., 1., &! ,'MAUNAKEA  		    '&! 	1086	  
    19.6920, -155.8700,   2523.00,  3.,  1., 1., &! ,'HUALALAI  		    '&! 	1087	  
    20.7080, -156.2500,   3055.00,  4.,  1., 1., &! ,'HALEAKALA 		    '&! 	1088	  
    21.7500, -158.7500,  -3000.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	1089	  
    23.5800, -163.8300,  -4000.00,  2.,  1., 1., &! ,'UNNAMED			    '&! 	1090	  
   -17.5700, -148.8500,  -1400.00,  2.,  1., 1., &! ,'TEAHITIA  		    '&! 	1091	  
   -17.6420, -148.6000,  -2100.00,  2.,  1., 1., &! ,'ROCARD			    '&! 	1092	  
   -18.3200, -148.6700,   -160.00,  2.,  1., 1., &! ,'MOUAPIHAA 		    '&! 	1093	  
   -17.8700, -148.0700,    435.00, -1.,  1., 1., &! ,'MEHETIA			    '&! 	1094	  
   -25.3700, -129.2700,    -39.00,  7.,  1., 1., &! ,'ADAMSSEAMOUNT		    '&! 	1095	  
   -28.9800, -140.2500,    -39.00,  2.,  5., 1., &! ,'MACDONALD 		    '&! 	1096	  
    16.5500, -105.3200,  -2700.00,  7.,  5., 1., &! ,'NORTHERNEPRSEGMENTRO2	    '&! 	10976	  
    15.8300, -105.4300,  -2300.00,  7.,  5., 1., &! ,'NORTHERNEPRSEGMENTRO3	    '&! 	10987	  
    10.7300, -103.5800,      0.00,  1.,  5., 1., &! ,'UNNAMED			    '&! 	1099	  
     9.8300, -104.3000,  -2500.00,  1.,  5., 1., &! ,'UNNAMED			    '&! 	1100	  
     0.7920,  -86.1500,  -2430.00,  2.,  5., 1., &! ,'GALAPAGOSRIFT		    '&! 	1101	  
    -8.2700, -107.9500,  -2800.00,  2.,  5., 1., &! ,'UNNAMED			    '&! 	1102	  
   -17.4360, -113.2060,  -2566.00,  2.,  5., 1., &! ,'SOUTHERNEPRSEGMENTK	    '&! 	1103	  
   -18.1750, -113.3500,  -2650.00,  3.,  5., 1., &! ,'SOUTHERNEPRSEGMENTJ	    '&! 	1104	  
   -18.5300, -113.4200,  -2600.00,  2.,  5., 1., &! ,'SOUTHERNEPRSEGMENTI	    '&! 	1105	  
   -49.6800,  178.7700,    402.00, -2.,  5., 1., &! ,'ANTIPODESISLAND		    '&! 	1106	  
   -53.9000, -140.3000,  -1000.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	1107	  
   -55.9700, -143.1700,      0.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	1108	  
    32.4180, -115.3050,    223.00, -2.,  1., 1., &! ,'PRIETOCERRO		    '&! 	1109	  
    31.7720, -113.4980,   1200.00, -1.,  1., 1., &! ,'PINACATE  		    '&! 	1110	  
    30.4680, -115.9960,    260.00, -2.,  1., 1., &! ,'SANQUINTINVOLCFIELD	    '&! 	1111	  
    29.9700, -114.4000,    180.00, -1.,  1., 1., &! ,'SANLUISISLA		    '&! 	1112	  
    29.3300, -114.5000,    960.00, -1.,  1., 1., &! ,'JARAGUAYVOLCFIELD 	    '&! 	1113	  
    29.0800, -113.5130,    440.00, -1.,  5., 1., &! ,'CORONADO  		    '&! 	1114	  
    29.0700, -118.2800,   1100.00, -1.,  5., 1., &! ,'GUADALUPE 		    '&! 	1115	  
    28.5000, -113.7500,   1360.00, -1.,  1., 1., &! ,'SANBORJAVOLCFIELD 	    '&! 	1116	  
    28.0000, -115.0000,      0.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	1117	  
    27.4300, -111.8800,    210.00, -1.,  1., 1., &! ,'TORTUGAISLA		    '&! 	1118	  
    26.0000, -111.9200,    780.00, -2.,  5., 1., &! ,'COMONDULAPURISIMA 	    '&! 	1119	  
    27.4700, -112.5910,   1940.00, -2.,  5., 1., &! ,'TRESVIRGENES		    '&! 	1120	  
    18.7800, -110.9500,   1050.00,  2.,  1., 1., &! ,'SOCORRO			    '&! 	1121	  
    24.1500, -104.4500,   2075.00, -1.,  1., 1., &! ,'DURANGOVOLCFIELD  	    '&! 	1122	  
    21.4500, -104.7300,   2340.00, -1.,  5., 1., &! ,'SANGANGUEY		    '&! 	1123	  
    19.3000, -110.8200,    332.00,  2.,  1., 1., &! ,'BARCENA			    '&! 	1124	  
    20.6200, -104.8300,   2560.00, -1.,  1., 1., &! ,'MASCOTAVOLCFIELD  	    '&! 	1125	  
    21.1250, -104.5080,   2280.00,  3.,  7., 1., &! ,'CEBORUCO  		    '&! 	1126	  
    19.5140, -103.6200,   3850.00,  1.,  5., 1., &! ,'COLIMA			    '&! 	1127	  
    19.4000, -100.2500,   3500.00,  7.,  1., 1., &! ,'ZITACUAROVALLEDEBRAVO	    '&! 	11287	  
    19.7300,  -99.7580,   3900.00,  6.,  5., 1., &! ,'JOCOTITLAN		    '&! 	1129	  
    19.8500, -101.7500,   3860.00,  2.,  3., 1., &! ,'MICHOACANGUANAJUATO	    '&! 	1130	  
    19.1080,  -99.7580,   4680.00,  7.,  7., 1., &! ,'TOLUCANEVADODE		    '&! 	1131	  
    19.3080,  -98.7000,   3600.00, -1.,  5., 1., &! ,'PAPAYO			    '&! 	1132	  
    19.1790,  -98.6420,   5230.00, -1.,  5., 1., &! ,'IZTACCIHUATL		    '&! 	1133	  
    19.0800,  -99.1300,   3930.00,  6.,  1., 1., &! ,'CHICHINAUTZIN		    '&! 	1134	  
    19.2310,  -98.0320,   4461.00,  7.,  5., 1., &! ,'MALINCHELA		    '&! 	1135	  
    19.2700,  -97.4700,   3485.00, -1.,  5., 1., &! ,'SERDANORIENTAL		    '&! 	1136	  
    19.6800,  -97.4500,   3150.00, -2.,  5., 1., &! ,'HUMEROSLOS		    '&! 	1137	  
    19.8090,  -96.5260,    800.00, -1.,  1., 1., &! ,'ATLIXCOSLOS		    '&! 	1138	  
    19.6700,  -96.7500,   2000.00,  7.,  1., 1., &! ,'NAOLINCOVOLCFIELD 	    '&! 	1139	  
    19.4920,  -97.1500,   4282.00,  6.,  1., 1., &! ,'COFREDEPEROTE		    '&! 	1140	  
    19.3300,  -97.2500,   3500.00, -1.,  1., 1., &! ,'GLORIALA  		    '&! 	1141	  
    19.1500,  -97.2700,   3940.00,  7.,  5., 1., &! ,'CUMBRESLAS		    '&! 	1142	  
    19.0230,  -98.6220,   5426.00,  1.,  6., 1., &! ,'POPOCATEPETL		    '&! 	1143	  
    19.0300,  -97.2680,   5675.00,  3.,  5., 1., &! ,'ORIZABAPICODE		    '&! 	1144	  
    18.5700,  -95.2000,   1650.00,  4.,  1., 1., &! ,'SANMARTIN 		    '&! 	1145	  
    17.3600,  -93.2280,   1150.00,  2.,  5., 1., &! ,'CHICHONEL 		    '&! 	1146	  
    15.1300,  -92.1120,   4060.00,  2.,  5., 1., &! ,'TACANA			    '&! 	1147	  
    15.0340,  -91.9030,   4220.00, -1.,  5., 1., &! ,'TAJUMULCO 		    '&! 	1148	  
    14.7560,  -91.5520,   3772.00,  1.,  5., 1., &! ,'SANTAMARIA		    '&! 	1149	  
    14.8200,  -91.4800,   3197.00,  3.,  5., 1., &! ,'ALMOLONGA 		    '&! 	1150	  
    14.5830,  -91.1860,   3535.00,  3.,  6., 1., &! ,'ATITLAN			    '&! 	1151	  
    14.6120,  -91.1890,   3158.00, -1.,  1., 1., &! ,'TOLIMAN			    '&! 	1152	  
    14.5010,  -90.8760,   3976.00,  2.,  6., 1., &! ,'ACATENANGO		    '&! 	1153	  
    14.4730,  -90.8800,   3763.00,  1.,  3., 1., &! ,'FUEGO			    '&! 	1154	  
    14.4650,  -90.7430,   3760.00, -1.,  5., 1., &! ,'AGUA			    '&! 	1155	  
    14.3300,  -90.4000,   1454.00, -1.,  5., 1., &! ,'CUILAPABARBARENA  	    '&! 	1156	  
    14.3810,  -90.6010,   2552.00,  1.,  2., 1., &! ,'PACAYA			    '&! 	1157	  
    14.3360,  -90.2690,   1815.00, -2.,  5., 1., &! ,'JUMAYTEPEQUE		    '&! 	1158	  
    14.1560,  -90.4070,   1845.00,  7.,  5., 1., &! ,'TECUAMBURRO		    '&! 	1159	  
    14.0300,  -90.1000,   1662.00, -1.,  5., 1., &! ,'MOYUTA			    '&! 	1160	  
    14.3080,  -89.9920,   1600.00, -1.,  1., 1., &! ,'FLORES			    '&! 	1161	  
    14.4300,  -89.9000,   1716.00, -1.,  5., 1., &! ,'TAHUAL			    '&! 	1162	  
    14.3300,  -89.8700,   1192.00, -1.,  1., 1., &! ,'SANTIAGOCERRO		    '&! 	1163	  
    14.4000,  -89.7800,   2042.00, -1.,  5., 1., &! ,'SUCHITAN  		    '&! 	1164	  
    14.1200,  -89.7300,   1775.00, -1.,  5., 1., &! ,'CHINGO			    '&! 	1165	  
    14.4200,  -89.6800,   1292.00, -1.,  5., 1., &! ,'IXTEPEQUE 		    '&! 	1166	  
    14.5500,  -89.6300,   1650.00, -1.,  1., 1., &! ,'IPALA			    '&! 	1167	  
    14.8300,  -89.5500,   1192.00, -1.,  1., 1., &! ,'CHIQUIMULAVOLCFIELD	    '&! 	1168	  
    14.5700,  -89.4500,   1200.00, -1.,  1., 1., &! ,'QUEZALTEPEQUE		    '&! 	1169	  
    14.2700,  -89.4800,    781.00, -1.,  1., 1., &! ,'SANDIEGO  		    '&! 	1170	  
    14.0500,  -89.6500,    957.00, -1.,  1., 1., &! ,'SINGUILCERRO		    '&! 	1171	  
    13.8910,  -89.7860,   2036.00, -1.,  1., 1., &! ,'APANECARANGE		    '&! 	1172	  
    13.8530,  -89.6300,   2381.00,  1.,  3., 1., &! ,'SANTAANA  		    '&! 	1173	  
    13.8130,  -89.6330,   1950.00,  2.,  2., 1., &! ,'IZALCO			    '&! 	1174	  
    13.8700,  -89.5500,    746.00, -1.,  5., 1., &! ,'COATEPEQUECALDERA 	    '&! 	1175	  
    14.0200,  -89.2500,    665.00, -1.,  1., 1., &! ,'CINOTEPEQUECERRO  	    '&! 	1176	  
    13.9000,  -89.1200,   1438.00, -2.,  1., 1., &! ,'GUAZAPA			    '&! 	1177	  
    13.7340,  -89.2940,   1893.00,  2.,  3., 1., &! ,'SANSALVADOR		    '&! 	1178	  
    13.6720,  -89.0530,    450.00,  3.,  5., 1., &! ,'ILOPANGO  		    '&! 	1179	  
    13.7200,  -88.7700,    700.00, -1.,  1., 1., &! ,'APASTEPEQUEFIELD  	    '&! 	1180	  
    13.4350,  -88.5320,   1172.00, -2.,  1., 1., &! ,'TABURETE  		    '&! 	1181	  
    13.5950,  -88.8370,   2182.00, -1.,  5., 1., &! ,'SANVICENTE		    '&! 	1182	  
    13.4190,  -88.4710,   1449.00, -1.,  1., 1., &! ,'USULUTAN  		    '&! 	1183	  
    13.4700,  -88.4300,   1640.00, -1.,  1., 1., &! ,'TIGREEL			    '&! 	1184	  
    13.4940,  -88.5020,   1593.00, -1.,  5., 1., &! ,'TECAPA			    '&! 	1185	  
    13.4780,  -88.3300,   1300.00, -1.,  5., 1., &! ,'CHINAMECA 		    '&! 	1186	  
    13.4280,  -88.1050,    181.00, -1.,  1., 1., &! ,'ARAMUACALAGUNA		    '&! 	1187	  
    13.4340,  -88.2690,   2130.00,  1.,  6., 1., &! ,'SANMIGUEL 		    '&! 	1188	  
    13.2750,  -87.8450,   1225.00, -2.,  5., 1., &! ,'CONCHAGUA 		    '&! 	1189	  
    13.2290,  -87.7670,    505.00,  3.,  1., 1., &! ,'CONCHAGUITA		    '&! 	1190	  
    13.2720,  -87.6410,    783.00, -1.,  1., 1., &! ,'TIGREISLAEL		    '&! 	1191	  
    13.3300,  -87.6300,    640.00, -1.,  1., 1., &! ,'ZACATEGRANDEISLA  	    '&! 	1192	  
    14.9800,  -87.9800,   1090.00, -1.,  1., 1., &! ,'YOJOALAKE 		    '&! 	1193	  
    16.1000,  -86.9000,     74.00, -1.,  1., 1., &! ,'UTILAISLAND		    '&! 	1194	  
    12.9800,  -87.5700,    872.00,  3.,  7., 1., &! ,'COSIGUINA 		    '&! 	1195	  
    12.7020,  -87.0040,   1745.00,  1.,  6., 1., &! ,'SANCRISTOBAL		    '&! 	1196	  
    12.6020,  -86.8450,   1061.00,  1.,  6., 1., &! ,'TELICA			    '&! 	1197	  
    12.5500,  -86.7500,    832.00, -1.,  6., 1., &! ,'ROTA			    '&! 	1198	  
    12.5060,  -86.7020,    728.00,  2.,  3., 1., &! ,'NEGROCERRO		    '&! 	1199	  
    12.4950,  -86.6880,   1088.00,  2.,  1., 1., &! ,'PILASLAS  		    '&! 	1200	  
    12.2420,  -86.3420,    518.00,  7.,  5., 1., &! ,'APOYEQUE  		    '&! 	1201	  
    12.1200,  -86.3200,    360.00,  7.,  1., 1., &! ,'NEJAPAMIRAFLORES  	    '&! 	1202	  
    12.4220,  -86.5400,   1297.00,  2.,  3., 1., &! ,'MOMOTOMBO 		    '&! 	1203	  
    11.9200,  -85.9800,    300.00, -1.,  1., 1., &! ,'GRANADA			    '&! 	1204	  
    11.9840,  -86.1610,    635.00,  1.,  2., 1., &! ,'MASAYA			    '&! 	1205	  
    11.7300,  -85.8200,    629.00, -1.,  1., 1., &! ,'ZAPATERA  		    '&! 	1206	  
    11.8260,  -85.9680,   1344.00, -1.,  5., 1., &! ,'MOMBACHO  		    '&! 	1207	  
    11.5380,  -85.6220,   1700.00,  1.,  6., 1., &! ,'CONCEPCION		    '&! 	1208	  
    11.4460,  -85.5150,   1394.00, -1.,  5., 1., &! ,'MADERAS			    '&! 	1209	  
    13.1700,  -86.4000,    899.00, -2.,  1., 1., &! ,'ESTELI			    '&! 	1210	  
    12.5300,  -86.1420,    603.00, -2.,  1., 1., &! ,'CIGUATEPECERROEL  	    '&! 	1211	  
    12.3000,  -85.7300,    926.00, -2.,  1., 1., &! ,'LAJASLAS  		    '&! 	1212	  
    12.5300,  -83.8700,    201.00, -1.,  1., 1., &! ,'AZULVOLCAN		    '&! 	1213	  
    10.9800,  -85.4730,   1659.00, -2.,  5., 1., &! ,'OROSI			    '&! 	1214	  
    10.8300,  -85.3240,   1916.00,  2.,  7., 1., &! ,'RINCONDELAVIEJA		    '&! 	1215	  
    10.6730,  -85.0150,   1916.00, -1.,  5., 1., &! ,'TENORIO			    '&! 	1216	  
    10.4630,  -84.7030,   1670.00,  1.,  6., 1., &! ,'ARENAL			    '&! 	1217	  
    10.3000,  -84.3660,   2267.00, -1.,  5., 1., &! ,'PLATANAR  		    '&! 	1218	  
    10.7480,  -85.1530,   2028.00,  2.,  5., 1., &! ,'MIRAVALLES		    '&! 	1219	  
    10.2000,  -84.2330,   2708.00,  1.,  6., 1., &! ,'POAS			    '&! 	1220	  
    10.1350,  -84.1000,   2906.00,  7.,  5., 1., &! ,'BARVA			    '&! 	1221	  
     9.9790,  -83.8520,   3432.00,  2.,  6., 1., &! ,'IRAZU			    '&! 	1222	  
    10.0250,  -83.7670,   3340.00,  3.,  6., 1., &! ,'TURRIALBA 		    '&! 	1223	  
     8.8080,  -82.5430,   3474.00,  5.,  5., 1., &! ,'BARU			    '&! 	1224	  
     8.4700,  -80.8200,   1297.00,  5.,  1., 1., &! ,'YEGUADALA 		    '&! 	1225	  
     8.5800,  -80.1700,   1185.00, -2.,  5., 1., &! ,'VALLEEL			    '&! 	1226	  
     5.2060,  -75.3640,   3858.00,  7.,  5., 1., &! ,'ROMERAL			    '&! 	1227	  
     5.0920,  -75.3000,   4000.00,  4.,  5., 1., &! ,'BRAVOCERRO		    '&! 	1228	  
     4.8200,  -75.3700,   4950.00,  7.,  5., 1., &! ,'SANTAISABEL		    '&! 	1229	  
     4.8950,  -75.3220,   5321.00,  2.,  6., 1., &! ,'RUIZNEVADODEL		    '&! 	1230	  
     4.6700,  -75.3300,   5200.00,  2.,  7., 1., &! ,'TOLIMANEVADODEL		    '&! 	1231	  
     4.4800,  -75.3920,   2650.00,  6.,  5., 1., &! ,'MACHIN			    '&! 	1232	  
     2.9300,  -76.0300,   5364.00,  1.,  6., 1., &! ,'HUILANEVADODEL		    '&! 	1233	  
     2.1080,  -76.5920,   4400.00, -1.,  5., 1., &! ,'SOTARA			    '&! 	1234	  
     1.5700,  -76.7800,   4054.00, -2.,  5., 1., &! ,'PETACAS			    '&! 	1235	  
     2.3200,  -76.4000,   4650.00,  2.,  6., 1., &! ,'PURACE			    '&! 	1236	  
     1.4700,  -76.9200,   4150.00,  2.,  7., 1., &! ,'DOAJUANA  		    '&! 	1237	  
     1.2200,  -77.3700,   4276.00,  1.,  6., 1., &! ,'GALERAS			    '&! 	1238	  
     1.0800,  -77.6800,   4070.00,  7.,  5., 1., &! ,'AZUFRAL			    '&! 	1239	  
     0.9500,  -77.8700,   4764.00,  2.,  6., 1., &! ,'CUMBAL			    '&! 	1240	  
     0.8280,  -77.9640,   4445.00,  2.,  5., 1., &! ,'NEGRODEMAYASQUERCERRO	    '&! 	12410	  
     0.5520,  -77.5800,   3955.00,  7.,  5., 1., &! ,'SOCHE			    '&! 	1242	  
     0.4680,  -78.2870,   4106.00,  7.,  5., 1., &! ,'CHACHIMBIRO		    '&! 	1243	  
     0.3080,  -78.3640,   3246.00,  6.,  5., 1., &! ,'CUICOCHA  		    '&! 	1244	  
     0.2580,  -78.1830,   4609.00,  7.,  5., 1., &! ,'IMBABURA  		    '&! 	1245	  
     0.1300,  -78.2700,   4263.00, -2.,  7., 1., &! ,'MOJANDA			    '&! 	1246	  
     0.0290,  -77.9860,   5790.00,  4.,  5., 1., &! ,'CAYAMBE			    '&! 	1247	  
     0.0380,  -78.4630,   3356.00,  6.,  5., 1., &! ,'PULULAGUA 		    '&! 	1248	  
    -0.0770,  -77.6560,   3562.00,  1.,  7., 1., &! ,'REVENTADOR		    '&! 	1249	  
    -0.3530,  -78.6170,   4463.00,  7.,  5., 1., &! ,'ATACAZO			    '&! 	1250	  
    -0.3750,  -78.2500,   4643.00,  4.,  5., 1., &! ,'CHACANA			    '&! 	1251	  
    -0.1710,  -78.5980,   4784.00,  1.,  6., 1., &! ,'GUAGUAPICHINCHA		    '&! 	1252	  
    -0.4810,  -78.1410,   5753.00,  3.,  7., 1., &! ,'ANTISANA  		    '&! 	1253	  
    -0.6590,  -78.7140,   5248.00, -1.,  5., 1., &! ,'ILLINIZA  		    '&! 	1254	  
    -0.5380,  -77.6260,   3990.00,  3.,  5., 1., &! ,'SUMACO			    '&! 	1255	  
    -0.6770,  -78.4360,   5911.00,  2.,  7., 1., &! ,'COTOPAXI  		    '&! 	1256	  
    -0.8500,  -78.9000,   3914.00,  6.,  5., 1., &! ,'QUILOTOA  		    '&! 	1257	  
    -1.4640,  -78.8150,   6310.00,  6.,  5., 1., &! ,'CHIMBORAZO		    '&! 	1258	  
    -1.7800,  -78.6130,   3336.00, -2.,  1., 1., &! ,'LICTO			    '&! 	1259	  
    -1.4670,  -78.4420,   5023.00,  1.,  6., 1., &! ,'TUNGURAHUA		    '&! 	1260	  
    -2.0020,  -78.3410,   5230.00,  1.,  6., 1., &! ,'SANGAY			    '&! 	1261	  
    -0.0200,  -91.5460,    790.00,  6.,  1., 1., &! ,'ECUADOR			    '&! 	1262	  
    -0.3700,  -91.5500,   1476.00,  1.,  1., 1., &! ,'FERNANDINA		    '&! 	1263	  
     0.0200,  -91.3500,   1710.00,  2.,  1., 1., &! ,'WOLF			    '&! 	1264	  
    -0.1800,  -91.2800,   1330.00,  3.,  1., 1., &! ,'DARWIN			    '&! 	1265	  
    -0.4300,  -91.1200,   1130.00,  2.,  5., 1., &! ,'ALCEDO			    '&! 	1266	  
    -0.8300,  -91.1700,   1124.00,  1.,  1., 1., &! ,'NEGRASIERRA		    '&! 	1267	  
    -0.9200,  -91.4080,   1640.00,  1.,  1., 1., &! ,'AZULCERRO 		    '&! 	1268	  
     0.5800,  -90.7500,    780.00,  2.,  1., 1., &! ,'PINTA			    '&! 	1269	  
     0.3200,  -89.9580,     64.00, -1.,  1., 1., &! ,'GENOVESA  		    '&! 	1270	  
     0.3300,  -90.4700,    343.00,  2.,  1., 1., &! ,'MARCHENA  		    '&! 	1271	  
    -0.6200,  -90.3300,    864.00, -1.,  1., 1., &! ,'SANTACRUZ 		    '&! 	1272	  
    -0.2200,  -90.7700,    920.00,  2.,  1., 1., &! ,'SANTIAGO  		    '&! 	1273	  
    -0.8800,  -89.5000,    759.00, -1.,  1., 1., &! ,'SANCRISTOBAL		    '&! 	1274	  
   -14.2000,  -71.3300,   3923.00,  7.,  5., 1., &! ,'QUIMSACHATA		    '&! 	1275	  
   -15.0700,  -73.1800,   4980.00, -2.,  1., 1., &! ,'AUQUIHUATOCERRO		    '&! 	1276	  
   -15.3300,  -73.4500,   5522.00, -1.,  5., 1., &! ,'SARASARA  		    '&! 	1277	  
   -15.5200,  -72.6500,   6377.00, -1.,  5., 1., &! ,'COROPUNA  		    '&! 	1278	  
   -15.4200,  -72.3300,   4713.00,  6.,  1., 1., &! ,'ANDAHUAORCOPAMPA  	    '&! 	1279	  
   -15.8300,  -72.1300,   4550.00,  7.,  5., 1., &! ,'HUAMBO			    '&! 	1280	  
   -15.7800,  -71.8500,   5967.00,  1.,  5., 1., &! ,'SABANCAYA 		    '&! 	1281	  
   -16.1910,  -71.5300,   6057.00, -2.,  5., 1., &! ,'CHACHANINEVADO		    '&! 	1282	  
   -16.2610,  -71.7300,   2520.00, -1.,  1., 1., &! ,'NICHOLSONCERRO		    '&! 	1283	  
   -16.2940,  -71.4090,   5822.00,  2.,  5., 1., &! ,'MISTIEL			    '&! 	1284	  
   -16.3550,  -70.9030,   5672.00,  1.,  6., 1., &! ,'UBINAS			    '&! 	1285	  
   -16.7550,  -70.5950,   5408.00,  3.,  5., 1., &! ,'TICSANI			    '&! 	1286	  
   -16.6080,  -70.8500,   4850.00,  5.,  8., 1., &! ,'HUAYNAPUTINA		    '&! 	1287	  
   -17.0250,  -70.3580,   5815.00, -1.,  5., 1., &! ,'TUTUPACA  		    '&! 	1288	  
   -17.1800,  -70.2000,   5550.00,  2.,  5., 1., &! ,'YUCAMANE  		    '&! 	1289	  
   -17.4700,  -69.8130,   5650.00, -1.,  5., 1., &! ,'CASIRINEVADOS		    '&! 	1290	  
   -18.1000,  -69.5000,   5860.00,  7.,  5., 1., &! ,'TAAPACA			    '&! 	1291	  
   -18.1700,  -69.1500,   6348.00,  6.,  5., 1., &! ,'PARINACOTA		    '&! 	1292	  
   -17.7200,  -69.7700,   5980.00, -1.,  5., 1., &! ,'TACORA			    '&! 	1293	  
   -18.6200,  -68.7500,   4215.00, -1.,  5., 1., &! ,'TAMBOQUEMADO		    '&! 	1294	  
   -18.4200,  -69.0920,   6071.00,  2.,  6., 1., &! ,'GUALLATIRI		    '&! 	1295	  
   -19.1300,  -68.5300,   5430.00, -1.,  5., 1., &! ,'TATASABAYA		    '&! 	1296	  
   -19.4500,  -67.4200,   3650.00, -1.,  5., 1., &! ,'JAYUKHOTALAGUNA		    '&! 	1297	  
   -19.7800,  -66.4800,   5438.00, -2.,  5., 1., &! ,'NUEVOMUNDO		    '&! 	1298	  
   -19.1500,  -68.8300,   5550.00,  2.,  6., 1., &! ,'ISLUGA			    '&! 	1299	  
   -20.8500,  -68.2000,   5543.00, -2.,  5., 1., &! ,'PAMPALUXSAR		    '&! 	1300	  
   -20.7300,  -68.5500,   5163.00,  2.,  5., 1., &! ,'IRRUPUTUNCU		    '&! 	1301	  
   -20.9300,  -68.4800,   5407.00,  3.,  5., 1., &! ,'OLCAPARUMA		    '&! 	1302	  
   -21.7870,  -68.2370,   5846.00, -2.,  5., 1., &! ,'AZUFRECERRODEL		    '&! 	1303	  
   -21.3000,  -68.1800,   5868.00, -2.,  5., 1., &! ,'OLLAGUE			    '&! 	1304	  
   -21.8800,  -68.4000,   6145.00,  2.,  6., 1., &! ,'SANPEDRO  		    '&! 	1305	  
   -22.7200,  -67.8920,   5971.00, -1.,  5., 1., &! ,'SAIRECABUR		    '&! 	1306	  
   -22.8300,  -67.8800,   5916.00, -1.,  5., 1., &! ,'LICANCABUR		    '&! 	1307	  
   -22.8950,  -67.5660,   5598.00, -1.,  5., 1., &! ,'GUAYAQUES 		    '&! 	1308	  
   -23.0000,  -67.7500,   5703.00, -1.,  5., 1., &! ,'PURICOCOMPLEX		    '&! 	1309	  
   -23.2360,  -67.6450,   5631.00, -1.,  5., 1., &! ,'COLACHI			    '&! 	1310	  
   -23.3000,  -67.6200,   6046.00, -1.,  5., 1., &! ,'ACAMARACHI		    '&! 	1311	  
   -23.5200,  -67.6700,   4555.00, -2.,  5., 1., &! ,'OVEROCERRO		    '&! 	1312	  
   -23.5800,  -67.7000,   5778.00, -2.,  5., 1., &! ,'CHILIQUES 		    '&! 	1313	  
   -22.5500,  -67.8500,   5890.00,  3.,  5., 1., &! ,'PUTANA			    '&! 	1314	  
   -23.7430,  -67.5340,   5852.00, -1.,  5., 1., &! ,'CORDONDEPUNTASNEGRAS	    '&! 	1315	  
   -23.8200,  -67.7700,   5910.00, -2.,  5., 1., &! ,'MIIQUES			    '&! 	1316	  
   -23.8300,  -67.9500,   3550.00, -1.,  1., 1., &! ,'TUJLECERRO		    '&! 	1317	  
   -23.9500,  -67.7300,   4450.00, -2.,  5., 1., &! ,'CAICHINQUE		    '&! 	1318	  
   -23.9700,  -68.1300,   3116.00, -2.,  5., 1., &! ,'TILOCALAR 		    '&! 	1319	  
   -24.1800,  -68.2500,   3500.00, -1.,  1., 1., &! ,'NEGRILLAREL		    '&! 	1320	  
   -24.1880,  -68.0540,   6233.00, -2.,  5., 1., &! ,'PULAR			    '&! 	1321	  
   -24.2800,  -68.6000,   4109.00, -2.,  1., 1., &! ,'NEGRILLARLA		    '&! 	1322	  
   -24.4000,  -68.2500,   6051.00,  7.,  5., 1., &! ,'SOCOMPA			    '&! 	1323	  
   -23.3700,  -67.7300,   5592.00,  1.,  7., 1., &! ,'LASCAR			    '&! 	1324	  
   -25.0800,  -68.3700,   5447.00, -2.,  5., 1., &! ,'ESCORIALCERRO		    '&! 	1325	  
   -24.7200,  -68.5300,   6739.00,  3.,  5., 1., &! ,'LLULLAILLACO		    '&! 	1326	  
   -25.3300,  -68.5200,   5463.00, -1.,  5., 1., &! ,'CORDONDELAZUFRE		    '&! 	1327	  
   -25.4200,  -68.5800,   5401.00, -1.,  5., 1., &! ,'BAYOCERRO 		    '&! 	1328	  
   -26.4800,  -68.5800,   6127.00, -1.,  5., 1., &! ,'NEVADASIERRA		    '&! 	1329	  
   -26.8000,  -68.3700,   5890.00, -2.,  5., 1., &! ,'FALSOAZUFRE		    '&! 	1330	  
   -27.0420,  -68.2800,   6621.00, -2.,  5., 1., &! ,'INCAHUASINEVADODE 	    '&! 	1331	  
   -25.1700,  -68.5000,   5697.00, -1.,  5., 1., &! ,'LASTARRIA 		    '&! 	1332	  
   -27.1080,  -68.7200,   6190.00, -1.,  5., 1., &! ,'SOLOEL			    '&! 	1333	  
   -27.1200,  -68.5500,   6887.00,  6.,  5., 1., &! ,'OJOSDELSALADONEVADOS	    '&! 	1334	  
   -27.3000,  -69.1300,   6052.00, -2.,  5., 1., &! ,'COPIAPO			    '&! 	1335	  
   -24.0500,  -66.4800,   5500.00, -2.,  5., 1., &! ,'TUZGLECERRO		    '&! 	1336	  
   -24.2500,  -67.7700,   6082.00, -2.,  5., 1., &! ,'ARACAR			    '&! 	1337	  
   -25.1000,  -68.2700,      0.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	1338	  
   -26.0800,  -67.5000,   4000.00, -1.,  1., 1., &! ,'ANTOFAGASTADELASIERRA	    '&! 	13398	  
   -26.6200,  -68.3500,   6532.00, -1.,  5., 1., &! ,'CONDORCERROEL		    '&! 	1340	  
   -26.6200,  -68.1500,   5740.00, -1.,  5., 1., &! ,'PEINADO			    '&! 	1341	  
   -26.7700,  -67.7200,   4400.00, -1.,  5., 1., &! ,'ROBLEDO			    '&! 	1342	  
   -27.2000,  -68.5500,   6660.00, -1.,  5., 1., &! ,'TIPAS			    '&! 	1343	  
   -27.1500, -109.3800,    511.00, -1.,  1., 1., &! ,'EASTERISLAND		    '&! 	1344	  
   -26.2800,  -80.1200,    193.00, -1.,  1., 1., &! ,'SANFELIX  		    '&! 	1345	  
   -33.6580,  -78.8500,    922.00,  3.,  1., 1., &! ,'ROBINSONCRUSOE		    '&! 	1346	  
   -33.6200,  -76.8300,   -642.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	1347	  
   -33.4000,  -69.8000,   6000.00,  2.,  6., 1., &! ,'TUPUNGATITO		    '&! 	1348	  
   -34.1610,  -69.8330,   5264.00,  2.,  6., 1., &! ,'MAIPO			    '&! 	1349	  
   -34.6080,  -70.2950,   4860.00, -1.,  5., 1., &! ,'PALOMO			    '&! 	1350	  
   -34.6500,  -70.0500,   5189.00, -1.,  5., 1., &! ,'ATUELCALDERADEL		    '&! 	1351	  
   -34.9300,  -70.0000,   4999.00, -2.,  5., 1., &! ,'RISCOPLATEADO		    '&! 	1352	  
   -33.7820,  -69.8970,   5856.00,  2.,  6., 1., &! ,'SANJOSE			    '&! 	1353	  
   -34.8140,  -70.3520,   4280.00,  2.,  5., 1., &! ,'TINGUIRIRICA		    '&! 	1354	  
   -35.5580,  -70.4960,   3508.00, -1.,  5., 1., &! ,'CALABOZOS 		    '&! 	1355	  
   -35.2400,  -70.5700,   4107.00,  2.,  6., 1., &! ,'PLANCHONPETEROA		    '&! 	1356	  
   -35.5800,  -70.7500,   3953.00,  2.,  5., 1., &! ,'DESCABEZADOGRANDE 	    '&! 	1357	  
   -36.0200,  -70.5800,   3092.00, -1.,  5., 1., &! ,'MAULELAGUNADEL		    '&! 	1358	  
   -35.9890,  -70.8490,   3621.00, -1.,  5., 1., &! ,'SANPEDROPELLADO		    '&! 	1359	  
   -36.1930,  -71.1610,   3242.00,  7.,  5., 1., &! ,'LONGAVINEVADODE		    '&! 	1360	  
   -36.2860,  -71.0090,   2268.00, -1.,  5., 1., &! ,'BLANCASLOMAS		    '&! 	1361	  
   -36.4500,  -70.9200,   1890.00, -1.,  1., 1., &! ,'RESAGO			    '&! 	1362	  
   -36.4200,  -69.2000,   3680.00, -1.,  1., 1., &! ,'PAYUNMATRU		    '&! 	1363	  
   -36.5800,  -70.4200,   4709.00, -2.,  5., 1., &! ,'DOMUYO			    '&! 	1364	  
   -35.6530,  -70.7610,   3788.00,  2.,  7., 1., &! ,'AZULCERRO 		    '&! 	1365	  
   -36.7700,  -69.8200,   1435.00, -1.,  5., 1., &! ,'COCHIQUITOVOLCGROUP	    '&! 	1366	  
   -37.1420,  -70.0300,   3978.00, -1.,  5., 1., &! ,'TROMEN			    '&! 	1367	  
   -37.5700,  -69.6200,    970.00, -1.,  1., 1., &! ,'PUESTOCORTADERAS  	    '&! 	1368	  
   -36.8630,  -71.3770,   3212.00,  1.,  6., 1., &! ,'CHILLANNEVADOSDE  	    '&! 	1369	  
   -37.7300,  -70.9000,   2500.00, -2.,  5., 1., &! ,'TROCON			    '&! 	1370	  
   -37.4060,  -71.3490,   2979.00,  3.,  6., 1., &! ,'ANTUCO			    '&! 	1371	  
   -37.9200,  -71.4500,   3164.00,  2.,  6., 1., &! ,'CALLAQUI  		    '&! 	1372	  
   -38.2700,  -71.1000,   2143.00, -1.,  1., 1., &! ,'MARIAQUILAGUNA		    '&! 	1373	  
   -38.3100,  -71.6450,   2806.00, -1.,  5., 1., &! ,'TOLGUACA  		    '&! 	1374	  
   -37.8500,  -71.1700,   2997.00,  1.,  6., 1., &! ,'COPAHUE			    '&! 	1375	  
   -38.3770,  -71.5800,   2865.00,  2.,  7., 1., &! ,'LONQUIMAY 		    '&! 	1376	  
   -38.9700,  -71.5200,   2282.00,  6.,  5., 1., &! ,'SOLLIPULLI		    '&! 	1377	  
   -39.2500,  -71.7000,   1496.00,  7.,  1., 1., &! ,'CABURGUAHUELEMOLLE	    '&! 	1378	  
   -38.6920,  -71.7290,   3125.00,  1.,  6., 1., &! ,'LLAIMA			    '&! 	1379	  
   -39.5000,  -71.7000,   2360.00,  3.,  5., 1., &! ,'QUETRUPILLAN		    '&! 	1380	  
   -39.6330,  -71.5000,   3747.00,  6.,  5., 1., &! ,'LANIN			    '&! 	1381	  
   -39.8800,  -71.5800,   2139.00,  4.,  5., 1., &! ,'HUANQUIHUEGROUP		    '&! 	1382	  
   -39.4200,  -71.9300,   2847.00,  1.,  6., 1., &! ,'VILLARRICA		    '&! 	1383	  
   -39.9270,  -72.0270,   2422.00,  2.,  5., 1., &! ,'MOCHOCHOSHUENCO		    '&! 	1384	  
   -40.3500,  -72.0700,   1114.00,  2.,  3., 1., &! ,'CARRANLOSVENADOS  	    '&! 	1385	  
   -40.7700,  -71.9500,   2024.00, -1.,  5., 1., &! ,'PANTOJACERRO		    '&! 	1386	  
   -40.7710,  -72.1530,   1990.00,  7.,  5., 1., &! ,'ANTILLANCAGROUP		    '&! 	1387	  
   -40.5900,  -72.1170,   2236.00,  2.,  5., 1., &! ,'PUYEHUECORDONCAULLE	    '&! 	1388	  
   -40.9690,  -72.2640,   2493.00,  3.,  5., 1., &! ,'PUNTIAGUDOCORDONCENIZOS	    '&! 	1389388   
   -41.1570,  -71.8850,   3491.00, -2.,  5., 1., &! ,'TRONADOR  		    '&! 	1390	  
   -41.2500,  -72.2700,    506.00,  7.,  1., 1., &! ,'CAYUTUELAVIGUERIA 	    '&! 	1391	  
   -41.1000,  -72.4930,   2652.00,  3.,  6., 1., &! ,'OSORNO			    '&! 	1392	  
   -41.4000,  -72.0000,   1862.00, -1.,  5., 1., &! ,'CUERNOSDELDIABLO  	    '&! 	1393	  
   -41.7550,  -72.3960,   2187.00, -1.,  5., 1., &! ,'YATE			    '&! 	1394	  
   -41.8740,  -72.4310,   1572.00, -1.,  5., 1., &! ,'HORNOPIREN		    '&! 	1395	  
   -41.8800,  -72.5800,   1210.00, -1.,  5., 1., &! ,'APAGADO			    '&! 	1396	  
   -42.0200,  -70.1800,   1359.00, -1.,  1., 1., &! ,'CRATERBASALTVOLCFIELD	    '&! 	13976	  
   -41.3260,  -72.6140,   2003.00,  2.,  7., 1., &! ,'CALBUCO			    '&! 	1398	  
   -42.3770,  -72.5780,   1318.00,  2.,  6., 1., &! ,'HUEQUI			    '&! 	1399	  
   -42.8330,  -72.6460,   1122.00,  1.,  7., 1., &! ,'CHAITEN			    '&! 	1400	  
   -42.7930,  -72.4390,   2404.00,  3.,  6., 1., &! ,'MINCHINMAVIDA		    '&! 	1401	  
   -43.5000,  -72.8000,   2042.00,  7.,  5., 1., &! ,'YANTELES  		    '&! 	1402	  
   -43.7800,  -72.4700,      0.00, -1.,  1., 1., &! ,'PALENAVOLCGROUP		    '&! 	1403	  
   -44.0800,  -72.8800,   2400.00,  6.,  5., 1., &! ,'MELIMOYU  		    '&! 	1404	  
   -44.3000,  -72.5300,    524.00, -1.,  1., 1., &! ,'PUYUHUAPI 		    '&! 	1405	  
   -44.7000,  -73.0800,   1660.00,  4.,  5., 1., &! ,'MENTOLAT  		    '&! 	1406	  
   -45.0590,  -72.9840,   2090.00, -2.,  5., 1., &! ,'CAY			    '&! 	1407	  
   -45.1000,  -73.1700,   2960.00,  6.,  5., 1., &! ,'MACA			    '&! 	1408	  
   -45.9000,  -72.9700,   1905.00,  2.,  8., 1., &! ,'HUDSONCERRO		    '&! 	1409	  
   -46.1700,  -72.6700,      0.00, -2.,  1., 1., &! ,'RIOMURTA  		    '&! 	1410	  
   -47.2000,  -73.4800,   3437.00,  2.,  5., 1., &! ,'ARENALES  		    '&! 	1411	  
   -43.1800,  -72.8000,   2300.00,  7.,  5., 1., &! ,'CORCOVADO 		    '&! 	1412	  
   -49.3580,  -73.2800,   1500.00,  2.,  5., 1., &! ,'VIEDMA			    '&! 	1413	  
   -50.3300,  -73.7500,   2546.00,  7.,  5., 1., &! ,'AGUILERA  		    '&! 	1414	  
   -50.9640,  -73.5850,   1000.00,  2.,  5., 1., &! ,'RECLUS			    '&! 	1415	  
   -49.0200,  -73.5500,   3607.00,  2.,  6., 1., &! ,'LAUTARO			    '&! 	1416	  
   -52.3300,  -73.4000,   1758.00,  2.,  5., 1., &! ,'BURNEYMONTE		    '&! 	1417	  
   -52.0000,  -70.0000,    282.00,  7.,  1., 1., &! ,'PALEIAIKEVOLCFIELD	    '&! 	1418	  
   -54.9500,  -70.2500,    150.00,  3.,  5., 1., &! ,'FUEGUINO  		    '&! 	1419	  
    17.6300,  -63.2300,    887.00,  5.,  5., 1., &! ,'SABA			    '&! 	1420	  
    17.4780,  -62.9600,    601.00,  6.,  5., 1., &! ,'QUILLTHE  		    '&! 	1421	  
    17.3700,  -62.8000,   1156.00,  6.,  5., 1., &! ,'LIAMUIGA  		    '&! 	1422	  
    17.1500,  -62.5800,    985.00, -2.,  5., 1., &! ,'NEVISPEAK 		    '&! 	1423	  
    16.7200,  -62.1800,    915.00,  1.,  5., 1., &! ,'SOUFRIEREHILLS		    '&! 	1424	  
    16.0500,  -61.6700,   1467.00,  2.,  5., 1., &! ,'SOUFRIEREGUADELOUPE	    '&! 	1425	  
    15.6120,  -61.4300,    861.00, -1.,  5., 1., &! ,'DIABLESMORNEAUX		    '&! 	1426	  
    15.5030,  -61.3970,   1430.00, -2.,  5., 1., &! ,'DIABLOTINSMORNE		    '&! 	1427	  
    15.3070,  -61.3050,   1224.00,  2.,  5., 1., &! ,'WATTMORNE 		    '&! 	1428	  
    15.3700,  -61.3300,   1387.00,  6.,  5., 1., &! ,'TROISPITONSMORNE  	    '&! 	1429	  
    15.2550,  -61.3410,    940.00,  6.,  5., 1., &! ,'PLATPAYSMORNE		    '&! 	1430	  
    14.8200,  -61.1700,   1397.00,  2.,  7., 1., &! ,'PELEE			    '&! 	1431	  
    13.8300,  -61.0500,    777.00,  4.,  5., 1., &! ,'QUALIBOU  		    '&! 	1432	  
    13.3300,  -61.1800,   1220.00,  2.,  7., 1., &! ,'SOUFRIEREST.VINCENT	    '&! 	1433	  
    12.3000,  -61.6400,   -185.00,  1.,  7., 1., &! ,'KICKEMJENNY		    '&! 	1434	  
    12.1500,  -61.6700,    840.00, -1.,  5., 1., &! ,'STCATHERINE		    '&! 	1435	  
    64.8000,  -23.7800,   1448.00,  6.,  1., 1., &! ,'SNAEFELLSJOKULL		    '&! 	1436	  
    64.8700,  -23.2500,    647.00, -1.,  1., 1., &! ,'HELGRINDUR		    '&! 	1437	  
    64.8700,  -22.2300,   1063.00,  6.,  1., 1., &! ,'LJOSUFJOLL		    '&! 	1438	  
    63.8800,  -22.5000,    230.00,  3.,  1., 1., &! ,'REYKJANES 		    '&! 	1439	  
    63.9300,  -22.1000,    379.00,  6.,  1., 1., &! ,'KRISUVIK  		    '&! 	1440	  
    63.9200,  -21.8300,    621.00,  6.,  5., 1., &! ,'BRENNISTEINSFJOLL 	    '&! 	1441	  
    64.0730,  -21.2020,    540.00, -2.,  5., 1., &! ,'HROMUNDARTINDUR		    '&! 	1442	  
    64.0800,  -21.3200,    803.00,  6.,  1., 1., &! ,'HENGILL			    '&! 	1443	  
    64.0300,  -20.8700,    214.00,  7.,  5., 1., &! ,'GRIMSNES  		    '&! 	1444	  
    64.6000,  -20.5800,   1400.00,  7.,  1., 1., &! ,'PRESTAHNUKUR		    '&! 	1445	  
    64.7500,  -19.9800,   1360.00,  6.,  1., 1., &! ,'HVERAVELLIR		    '&! 	1446	  
    64.7800,  -18.9200,   1782.00, -1.,  1., 1., &! ,'HOFSJOKULL		    '&! 	1447	  
    63.4300,  -20.2800,    279.00,  2.,  1., 1., &! ,'VESTMANNAEYJAR		    '&! 	1448	  
    63.6300,  -19.6200,   1666.00,  3.,  5., 1., &! ,'EYJAFJOLL 		    '&! 	1449	  
    63.6300,  -19.0500,   1512.00,  2.,  1., 1., &! ,'KATLA			    '&! 	1450	  
    63.7800,  -19.5700,   1463.00, -2.,  1., 1., &! ,'TINDFJALLAJOKULL  	    '&! 	1451	  
    63.9200,  -19.1700,   1259.00,  6.,  5., 1., &! ,'TORFAJOKULL		    '&! 	1452	  
    63.9800,  -19.7000,   1491.00,  1.,  7., 1., &! ,'HEKLA			    '&! 	1453	  
    64.4200,  -17.3300,   1725.00,  1.,  1., 1., &! ,'GRIMSVOTN 		    '&! 	1454	  
    64.6300,  -17.5300,   2009.00,  2.,  1., 1., &! ,'BARDARBUNGA		    '&! 	1455	  
    64.7300,  -17.9200,   1535.00, -1.,  1., 1., &! ,'TUNGNAFELLSJOKULL 	    '&! 	1456	  
    64.6500,  -16.7200,   1929.00,  2.,  1., 1., &! ,'KVERKFJOLL		    '&! 	1457	  
    65.0300,  -16.7500,   1516.00,  2.,  1., 1., &! ,'ASKJA			    '&! 	1458	  
    65.4300,  -16.6500,    939.00,  7.,  1., 1., &! ,'FREMRINAMUR		    '&! 	1459	  
    65.7300,  -16.7800,    818.00,  2.,  2., 1., &! ,'KRAFLA			    '&! 	1460	  
    65.8800,  -16.8300,    564.00,  7.,  1., 1., &! ,'THEISTAREYKJARBUNGA	    '&! 	1461	  
    66.3000,  -17.1000,      0.00,  3.,  1., 1., &! ,'TJORNESFRACTUREZONE	    '&! 	1462	  
    64.0000,  -16.6500,   2119.00,  4.,  1., 1., &! ,'ORAEFAJOKULL		    '&! 	1463	  
    64.2700,  -16.6500,   1760.00,  2.,  5., 1., &! ,'ESJUFJOLL 		    '&! 	1464	  
    66.6700,  -18.5000,      5.00,  4.,  5., 1., &! ,'KOLBEINSEYRIDGE		    '&! 	1465	  
    71.0800,   -8.1700,   2277.00,  2.,  1., 1., &! ,'JANMAYEN  		    '&! 	1466	  
    88.2700,  -65.6000,  -1500.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	1467	  
    85.5800,   85.0000,  -3800.00, -1.,  1., 1., &! ,'UNNAMED			    '&! 	1468	  
    49.0000,  -34.5000,  -1650.00,  3.,  1., 1., &! ,'UNNAMED			    '&! 	1469	  
    39.9500,  -25.8300,  -2835.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	1470	  
    38.7500,  -38.0800,  -4200.00,  3.,  1., 1., &! ,'UNNAMED			    '&! 	1471	  
    39.4620,  -31.2160,    914.00,  7.,  1., 1., &! ,'FLORES			    '&! 	1472	  
    39.6990,  -31.1110,    718.00, -1.,  1., 1., &! ,'CORVO			    '&! 	1473	  
    38.6000,  -28.7300,   1043.00,  2.,  5., 1., &! ,'FAYAL			    '&! 	1474	  
    38.4700,  -28.4000,   2351.00,  4.,  1., 1., &! ,'PICO			    '&! 	1475	  
    38.6500,  -28.0800,   1053.00,  2.,  1., 1., &! ,'SANJORGE  		    '&! 	1476	  
    39.0200,  -27.9700,    402.00, -1.,  1., 1., &! ,'GRACIOSA  		    '&! 	1477	  
    38.7300,  -27.3200,   1023.00,  1.,  1., 1., &! ,'TERCEIRA  		    '&! 	1478	  
    38.2300,  -26.6300,    -13.00,  4.,  1., 1., &! ,'DONJOAODECASTROBANK	    '&! 	1479	  
    37.7800,  -25.6700,    350.00,  5.,  1., 1., &! ,'PICOSVOLCSYSTEM		    '&! 	1480	  
    37.8700,  -25.7800,    856.00,  3.,  5., 1., &! ,'SETECIDADES		    '&! 	1481	  
    37.7700,  -25.4700,    947.00,  5.,  5., 1., &! ,'AGUADEPAU 		    '&! 	1482	  
    37.7700,  -25.3200,    805.00,  5.,  5., 1., &! ,'FURNAS			    '&! 	1483	  
    37.6000,  -25.8800,   -197.00,  2.,  5., 1., &! ,'MONACOBANK		    '&! 	1484	  
    32.7300,  -16.9700,   1862.00,  7.,  1., 1., &! ,'MADEIRA			    '&! 	1485	  
    28.5700,  -17.8300,   2426.00,  2.,  1., 1., &! ,'LAPALMA			    '&! 	1486	  
    27.7300,  -18.0300,   1500.00,  7.,  1., 1., &! ,'HIERRO			    '&! 	1487	  
    28.2710,  -16.6410,   3715.00,  2.,  5., 1., &! ,'TENERIFE  		    '&! 	1488	  
    28.0000,  -15.5800,   1950.00,  7.,  1., 1., &! ,'GRANCANARIA		    '&! 	1489	  
    28.3580,  -14.0200,    529.00, -1.,  1., 1., &! ,'FUERTEVENTURA		    '&! 	1490	  
    29.0300,  -13.6300,    670.00,  3.,  1., 1., &! ,'LANZAROTE 		    '&! 	1491	  
    14.9500,  -24.3500,   2829.00,  2.,  5., 1., &! ,'FOGO			    '&! 	1492	  
    14.8500,  -24.7200,    900.00, -1.,  5., 1., &! ,'BRAVA			    '&! 	1493	  
    16.8500,  -24.9700,    725.00, -1.,  5., 1., &! ,'SAOVICENTE		    '&! 	1494	  
     7.0000,  -21.8300,  -1415.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	1495	  
     4.2000,  -21.4500,  -2900.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	1496	  
    -0.7200,  -20.5300,  -1528.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	1497	  
    -3.5000,  -24.5000,  -5300.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	1498	  
    -7.9500,  -14.3700,    858.00, -1.,  1., 1., &! ,'ASCENSION 		    '&! 	1499	  
   -20.5140,  -29.3310,    600.00, -1.,  5., 1., &! ,'TRINDADE  		    '&! 	1500	  
   -37.4200,  -12.4800,    365.00,  1.,  5., 1., &! ,'NIGHTINGALEISLAND 	    '&! 	1501	  
   -37.0920,  -12.2800,   2060.00,  2.,  2., 1., &! ,'TRISTANDACUNHA		    '&! 	1502	  
   -54.4200,    3.3500,    780.00,  7.,  1., 1., &! ,'BOUVET			    '&! 	1503	  
   -53.9300,    5.5000,      0.00, -2.,  5., 1., &! ,'THOMPSONISLAND		    '&! 	1504	  
   -66.4200,  162.4700,   1340.00, -1.,  5., 1., &! ,'YOUNGISLAND		    '&! 	1505	  
   -67.4000,  164.8300,   1167.00, -2.,  1., 1., &! ,'STURGEISLAND		    '&! 	1506	  
   -72.6700,  165.5000,   3040.00,  7.,  5., 1., &! ,'PLEIADESTHE		    '&! 	1507	  
   -73.4500,  164.5800,   2987.00, -2.,  1., 1., &! ,'UNNAMED			    '&! 	1508	  
   -74.3500,  164.7000,   2732.00,  4.,  5., 1., &! ,'MELBOURNE 		    '&! 	1509	  
   -76.8300,  163.0000,   -500.00, -2.,  5., 1., &! ,'UNNAMED			    '&! 	1510	  
   -66.7800,  163.2500,   1239.00,  3.,  1., 1., &! ,'BUCKLEISLAND		    '&! 	1511	  
   -78.2500,  163.3300,   3000.00, -2.,  1., 1., &! ,'ROYALSOCIETYRANGE 	    '&! 	1512	  
   -76.0500, -136.0000,   3478.00,  7.,  5., 1., &! ,'BERLIN			    '&! 	1513	  
   -75.8000, -132.3300,   2978.00, -2.,  5., 1., &! ,'ANDRUS			    '&! 	1514	  
   -77.1700, -126.8800,   3292.00, -2.,  1., 1., &! ,'WAESCHE			    '&! 	1515	  
   -73.4300, -126.6700,   3110.00, -2.,  1., 1., &! ,'SIPLE			    '&! 	1516	  
   -75.8000, -115.8300,   3595.00, -2.,  1., 1., &! ,'TONEYMOUNTAIN		    '&! 	1517	  
   -76.2800, -112.0800,   3460.00,  7.,  5., 1., &! ,'TAKAHE			    '&! 	1518	  
   -74.3300,  -99.4200,    749.00, -2.,  1., 1., &! ,'HUDSONMOUNTAINS		    '&! 	1519	  
   -68.8500,  -90.5800,   1640.00, -1.,  1., 1., &! ,'PETERIISLAND		    '&! 	1520	  
   -77.5300,  167.1700,   3794.00,  1.,  6., 1., &! ,'EREBUS			    '&! 	1521	  
   -62.1000,  -57.9300,    180.00,  2.,  5., 1., &! ,'PENGUINISLAND		    '&! 	1522	  
   -62.9700,  -60.6500,    576.00,  2.,  7., 1., &! ,'DECEPTIONISLAND		    '&! 	1523	  
   -63.5800,  -55.7700,    353.00, -1.,  1., 1., &! ,'PAULET			    '&! 	1524	  
   -62.0500,  -56.7500,    240.00, -2.,  5., 1., &! ,'BRIDGEMANISLAND		    '&! 	1525	  
   -65.0300,  -60.0500,    368.00, -2.,  1., 1., &! ,'SEALNUNATAKSGROUP 	    '&! 	1526	  
   -59.4500,  -27.3700,   1075.00,  2.,  5., 1., &! ,'THULEISLANDS		    '&! 	1527	  
   -58.4200,  -26.3300,   1370.00,  1.,  5., 1., &! ,'MONTAGUISLAND		    '&! 	1528	  
   -59.0300,  -26.5800,   1100.00,  2.,  6., 1., &! ,'BRISTOLISLAND		    '&! 	1529	  
   -57.7800,  -26.4500,    990.00,  1.,  6., 1., &! ,'MICHAEL			    '&! 	1530	  
   -57.0800,  -26.6700,    550.00,  2.,  5., 1., &! ,'CANDLEMASISLAND		    '&! 	1531	  
   -56.7000,  -27.1500,   1005.00, -1.,  5., 1., &! ,'HODSON			    '&! 	1532	  
   -56.6700,  -28.1300,    190.00, -1.,  5., 1., &! ,'LESKOVISLAND		    '&! 	1533	  
   -56.3000,  -27.5700,    551.00,  3.,  5., 1., &! ,'ZAVODOVSKI		    '&! 	1534	  
   -55.9200,  -28.0800,    -27.00,  2.,  5., 1.  &! ,'PROTECTORSHOAL		    '&! 	1535	  

        /),(/6,nvolcanoes/))
	
	
!emission data	
	
real,parameter, dimension(5,11) :: emiss_data=RESHAPE((/&
!------------------------------------------------------------------------------------------------------------------------------------
!    H km     |Duration|  Eruption   | Volume	  | mass fraction   | ESP   |    Type  |  Example					
! above vent  |  hr    |  rate (kg/s)|  (km3)	  | < 63 micron     |	    |	       |							
!------------------------------------------------------------------------------------------------------------------------------------
      7.,       60.,    1.E+05,      0.01  ,	    0.05,	   &!1   M0 !Standard mafic	    !	Cerro Negro, Nicaragua, 4/13/1992	 
      2.,       100.,	5.E+03,      0.001 ,	    0.02,	   &!2   M1 !small mafic	 !   Etna, Italy, 7/19-24/2001  	      
      7.,       60.,	1.E+05,      0.01  ,	    0.05,	   &!3   M2 !medium mafic	 !   Cerro Negro, Nicaragua,  4/9-13/1992     
     10.,       5.,	1.E+06,      0.17  ,	    0.1 ,	   &!4   M3 !large mafic	 !   Fuego, Guatemala, 10/14/1974	      
     11.,       3.,	4.E+06,      0.015 ,	    0.4 ,	   &!5   S0 !standard silicic	 !   Spurr, USA, 8/18/1992	      
      5.,       12.,	2.E+05,      0.003 ,	    0.1 ,	   &!6   S1 !small silicic	 !   Ruapehu, New Zealand, 6/17/1996	      
     11.,       3.,	4.E+06,      0.015 ,	    0.4 ,	   &!7   S2 !medium silicic	 !   Spurr, USA, 8/18/1992		      
     15.,       8.,	1.E+07,      0.15  ,	    0.5 ,	   &!8   S3 !largnvolcanoese silicic	 !   St. Helens, USA,  5/18/1980	      
     25.,       0.5,	1.E+08,      0.05  ,	    0.5 ,	   &!9   S8 !co-ignimbrite silicic!   St. Helens, USA, 5/18/1980 (pre-9AM)
     10.,       0.01,   3.E+06,      0.0003,	    0.6	,          &!10  S9 !Brief silicic	 !   Soufri�re Hills, Montserrat (composite)  
      0.,       -1.,   -1.,         -1.,	    -1.	           &!11  U0 !default submarine    !   none			   
!------------------------------------------------------------------------------------------------------------------------------------
        /),(/5,11/))
!-----------------------------------------------------------------------------------------------------------------------!
!                                    P. Webley - size distribution 
!															!
!Ash bin	Min      Max 	 phi	M0	M1	M2	M3	S0	S1	S2	S3	S8	S9	!	 
!             Size (um)	Size (um)											!
!1	   	 1000	 2000	 -1	6.5	 0	 6.5	 13	 22	 24	 22	 2.92	 2.92	 0	!	  
!2	   	 500	 1000	 0	12	4	12	20	5	25	5	3.55	3.55	0	!	 
!3	   	 250	 500	 1	18.75	10	18.75	27.5	4	20	4	11.82	11.82	0	!	 
!4	   	 125	 250	 2	36.25	50	36.25	22.5	5	12	5	8.24	8.24	9	!	 
!5	   	 62.5	 125	 3	20.5	34	20.5	7	24.5	9	24.5	7.9	7.9	22	!	 
!6	   	 31.25   62.5	 4	3	2	3	4	12	4.25	12	13.02	13.02	23	!	 
!7	   	 15.625  31.25   5	1.5	0	1.5	3	11	3.25	11	16.28	16.28	21	!	 
!8	   	 7.8125  15.625  6	1	0	1	2	8	1.25	8	15.04	15.04	18	!	 
!9	   	 3.90625 7.8125  7	0.5	0	0.5	1	5	0.75	5	10.04	10.04	7	!	 
!10	   	 0	 3.90625 8	0	0	0	0	3.5	0.5	3.5	11.19	11.19	0	!	 
!															!
!		Less than 63 um		6%	2%	6%	10%	40%	10%	40%	66%	66%	69%	!
!   													                !
!   Reference/notes	    M0 = M2 Scollo et al 2007	    Interpolated from M1 and M3     Rose et al (2007)	        ! 
!   Durant and Rose (2009 JVGR)     Bonadonna and Houghton 2005     Durant and Rose (2009 JVGR) 	    		!
!   Durant et al 2009	    Durant et al 2009	    Bonadonna et al 2002				    		!
!   													    		!
!   ESP 	    5%      2%      5%      10%     40%     10%     40%     50%     50%     60% 	    		!
!   "(mastin et al, JVGR)"										    		!
!   													    		!
!   Total	    100     100     100     100     100     100     100     100     100     100 	    		!
!															!
!															!
real,parameter, dimension(10,11) :: ash_size_dist_data=RESHAPE((/&                                                           !
!-----------------------------------------------------------------------------------------------------------------------!
! Ash bin 1     2        3      4       5      6       7       8      9    10                                           !
!-----------------------------------------------------------------------------------------------------------------------!
       6.50 ,  12.00,  18.75, 36.25,  20.50,  3.00,   1.50,  1.00,  0.50,  0.00, &!1   M0                               !
       0.00 ,	4.00,  10.00, 50.00,  34.00,  2.00,   0.00,  0.00,  0.00,  0.00, &!2   M1 				!
       6.50 ,  12.00,  18.75, 36.25,  20.50,  3.00,   1.50,  1.00,  0.50,  0.00, &!3   M2 				!
      13.00 ,  20.00,  27.50, 22.50,   7.00,  4.00,   3.00,  2.00,  1.00,  0.00, &!4   M3 				!
      22.00 ,	5.00,	4.00,  5.00,  24.50, 12.00,  11.00,  8.00,  5.00,  3.50, &!5   S0 				!
      24.00 ,  25.00,  20.00, 12.00,   9.00,  4.25,   3.25,  1.25,  0.75,  0.50, &!6   S1 				!
      22.00 ,	5.00,	4.00,  5.00,  24.50, 12.00,  11.00,  8.00,  5.00,  3.50, &!7   S2 				!
       2.92 ,	3.55,  11.82,  8.24,   7.90, 13.02,  16.28, 15.04, 10.04, 11.19, &!8   S3 				!
       2.92 ,	3.55,  11.82,  8.24,   7.90, 13.02,  16.28, 15.04, 10.04, 11.19, &!9   S8 				!
       0.00 ,	0.00,	0.00,  9.00,  22.00, 23.00,  21.00, 18.00,  7.00,  0.00, &!10  S9 				!
       0.00 ,	0.00,	0.00,  0.00,   0.00,  0.00,   0.00,  0.00,  0.00,  0.00  &!11  U0 				!
!-----------------------------------------------------------------------------------------------------------------------!
        /),(/10,11/))
!-----------------------------------------------------------------------------------------------------------------------!
  real :: ash_size_dist(10) ! 10 = # of bins

  integer :: &
       inje_altitude_km   = 01, & 
       duration_hr        = 02, & 
       eruption_rate_kgs  = 03, & 
       volume_km3         = 04, & 
       massfraction_63    = 05    
 real, parameter :: particle_density=2500. ! kg/m3)
 
!-  convertion to SI
! inje_altitude_meters = inje_altitude_km  *1000 
! duration_seconds     = duration_hr * 3600      
! eruption_rate_kgs    = ok
! volume_m3            = volume_km3 * 1000**3     
  integer, parameter :: maxnspecies = 200 , nspecies = 2 ! - only 2 species for now 
  integer, parameter ::         &
   ASH  	    = 1 & 
  ,SO2  	    = 2 &
  ,CO2  	    = 3 &
  ,HCl  	    = 4 &
  ,HF		    = 5 &
  ,H2O  	    = 6 
  character(LEN=25),dimension(nspecies),parameter :: spc_name= &
  ! '1234567890123456789012345'
  (/                          & 
  'ASH                       '&
 ,'SO2                       '&
! ,'CO2                      '&
! ,'HCl  	             '&
! ,'HF		             '&
! ,'H2O                      '&
  /)
!---
  type volcanoes_vars   
     real, pointer, dimension(:,:,:)  :: src
  end type volcanoes_vars

  type (volcanoes_vars), allocatable :: volcanoes_g(:)
!---  
  type volc_vars  
     real, pointer, dimension(:)  :: volcano
  end type volc_vars

  type (volc_vars), allocatable :: volc_prop(:)

  integer, parameter ::  nprop = 10

  integer, parameter ::         &
  LATI  	  =1  &
 ,LONG  	  =2  &
 ,ELEV  	  =3  &
 ,BEGT  	  =4  &
 ,CRAT  	  =5  &
 ,HEAT  	  =6  &
 ,INJH  	  =7  &
 ,DURA  	  =8  & 
 ,MASH  	  =9  &
 ,MSO2  	  =10
!---
  type volc_vars_P  
     real, pointer, dimension(:,:)  :: prop
  end type volc_vars_P

  type (volc_vars_P), allocatable :: volcanoesP_g(:)

contains

	
  !---------------------------------------------------------------

  subroutine alloc_volcanoes(volcanoes,volcanoesP,n1,n2,n3)

    implicit none

    type (volcanoes_vars),dimension(nspecies)  :: volcanoes
    type (volc_vars_P),dimension(nprop)  :: volcanoesP
    integer,intent(in) :: n1,n2,n3
    integer ispc,iprop
    
    do ispc=1,nspecies
     allocate (volcanoes(ispc)%src(n1,n2,n3))
      volcanoes_g(ispc)%src(:,:,:)=0.
    enddo
    do iprop=1,nprop
     allocate (volcanoesP(iprop)%prop(n1,n2))
      volcanoesP_g(iprop)%prop(:,:)=0.
    enddo

    return
  end subroutine alloc_volcanoes

  !---------------------------------------------------------------

  subroutine nullify_volcanoes(volcanoes,volcanoesP)

    implicit none

    type (volcanoes_vars),dimension(nspecies)  :: volcanoes
    type (volc_vars_P)   ,dimension(nprop)  :: volcanoesP
    integer ispc,iprop

    do ispc=1,nspecies
       if (associated(volcanoes(ispc)%src))    nullify (volcanoes(ispc)%src)
    enddo
    do iprop=1,nprop
     if (associated(volcanoesP(iprop)%prop))    nullify (volcanoesP(iprop)%prop)
    enddo

    return
  end subroutine nullify_volcanoes

  !---------------------------------------------------------------

  subroutine alloc_volc_prop(volc_prop)

    implicit none

    type (volc_vars),dimension(nprop)  :: volc_prop
    integer i
    
    do i=1,nprop
     allocate (volc_prop(i)%volcano(nvolcanoes))
    enddo

    return
  end subroutine alloc_volc_prop

  !---------------------------------------------------------------

  subroutine nullify_volc_prop(volc_prop)

    implicit none

    type (volc_vars),dimension(nprop)  :: volc_prop
    integer i

    do i=1,nprop
       if (associated(volc_prop(i)%volcano))   nullify (volc_prop(i)%volcano)
    enddo

    return
  end subroutine nullify_volc_prop


end module volcanoes_emissions
!------------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------------
 
  subroutine mem_volcanoes(n1,n2,n3)
    use volcanoes_emissions
    implicit none
    integer i,j
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(volcanoes_g )) allocate(volcanoes_g (nspecies))
    if(.not. allocated(volcanoesP_g)) allocate(volcanoesP_g(nprop))
   
    call nullify_volcanoes(volcanoes_g(:),volcanoesP_g(:))      
    call alloc_volcanoes  (volcanoes_g(:),volcanoesP_g(:),n1,n2,n3) 
    
    
  end subroutine mem_volcanoes
  !---------------------------------------------------------------
   subroutine mem_volc_prop()
    use volcanoes_emissions
    implicit none
    integer i

     if(.not. allocated(volc_prop)) allocate(volc_prop(nprop))
!   
    call nullify_volc_prop(volc_prop(:))      
    call alloc_volc_prop  (volc_prop(:)) 
  end subroutine mem_volc_prop

  !---------------------------------------------------------------  
  subroutine process_volcanoes(iyear,imon,iday,ihour,ng,ngrids,n1,n2,n3,rlat,rlon,rland&
                        ,deltax,deltay,xt,yt,xm,ym,plat,plon)
  use grid_dims_out, only : volcano_index,use_these_values
  use volcanoes_emissions, only :volcanoes_name,	volc_prop,nvolcanoes,nspecies,&	
  	 LATI,LONG,ELEV,BEGT,CRAT,HEAT,INJH,DURA,MASH,MSO2
  implicit none
  integer, intent (in) :: iyear,imon,iday,ihour,ng,n1,n2,n3,ngrids
  real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
  real, intent (in) :: deltax,deltay
  real, intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2),plat,plon
  real, dimension(nvolcanoes,nspecies) :: qsc
  integer i1,i2,ii
  print*,"==============================================================" 
! print*,"==> processing volcanic eruption for: ",  trim(volcanoes_name(volcano_index))

  if(ng == 1) then ! just for the first grid is needed

  !- allocate memory for volc properties.
    call mem_volc_prop ()

  !-- process the source emission estimate for each valid volc and accumulate per
  !-- model grid box, if is it the case.
if(volcano_index==0) then 
 i1=1
 i2=nvolcanoes
else
 i1=volcano_index
 i2=volcano_index
endif

!   call get_emission_per_volcano(volcano_index,use_these_values&
do ii = i1,i2
  print*,"==> processing volcanic eruption for: ",  trim(volcanoes_name(ii))
    call get_emission_per_volcano(ii,use_these_values&
		 , qsc  		    &
		 , volc_prop(LONG)%volcano  &
                 , volc_prop(LATI)%volcano  &  
		 , volc_prop(ELEV)%volcano  &  
		 , volc_prop(BEGT)%volcano  &	 
		 , volc_prop(CRAT)%volcano  &
		 , volc_prop(HEAT)%volcano  &
		 , volc_prop(INJH)%volcano  &
		 , volc_prop(DURA)%volcano  &
		 , volc_prop(MASH)%volcano  &	 
		 , volc_prop(MSO2)%volcano  )	 
enddo

  endif !ng==1			
  !- performs the "gridding"
  call interp_volc_to_model(ng,n1,n2,n3,xt,yt,xm,ym,deltax,deltay,plat,plon&
                 , rlat,rlon,rland  &
                 , nspecies,volcano_index,nvolcanoes &
                 , qsc                      &
		 , volc_prop(LONG)%volcano  &
                 , volc_prop(LATI)%volcano  &  
		 , volc_prop(ELEV)%volcano  &  
		 , volc_prop(BEGT)%volcano  &	 
		 , volc_prop(CRAT)%volcano  &
		 , volc_prop(HEAT)%volcano  &
		 , volc_prop(INJH)%volcano  &
		 , volc_prop(DURA)%volcano  &
		 , volc_prop(MASH)%volcano  &	
		 , volc_prop(MSO2)%volcano  )	  
end subroutine process_volcanoes			
  !---------------------------------------------------------------
subroutine get_emission_per_volcano(volcano_index,use_these_values&
                           ,qsc,qlong,qlati,qelev,qBEGT,qcrat,qheat,qinjh &
                           ,qdura,QMASH,QMSO2)
use volcanoes_emissions !, only: spc_name,
 
implicit none
integer, intent(in) ::volcano_index
character(*), intent(in) ::use_these_values
real,    dimension(nvolcanoes) , intent(out):: qlong,qlati,qelev,qBEGT,qcrat &
                                              ,qheat,qinjh,qdura  ,QMASH , QMSO2
real,    dimension(nvolcanoes,nspecies), intent(out) :: qsc
real :: this_injh,this_dura,this_mash,this_so2
logical  :: there

integer i1,i2,iesp,ifoc

if(volcano_index==0) then 
 i1=1
 i2=nvolcanoes
else
 i1=volcano_index
 i2=volcano_index
endif
!- calculates the emission by volcano

do ifoc = i1,i2
      QLONG(ifoc)=volcanoes_data(longitude,ifoc)
      QLATI(ifoc)=volcanoes_data(Latitude ,ifoc)
      QELEV(ifoc)=volcanoes_data(Elevation,ifoc)
      QINJH(ifoc)=emiss_data(inje_altitude_km,int(volcanoes_data(esp,ifoc))) *1000.!meters
      QDURA(ifoc)=emiss_data(duration_hr     ,int(volcanoes_data(esp,ifoc))) *3600.!sec
      QMASH(ifoc)=emiss_data(volume_km3      ,int(volcanoes_data(esp,ifoc))) *1.e+9* &! vol m^3
                  particle_density ! kg/m^3
!not using "massfraction_63"  anymore
!                  emiss_data(massfraction_63 ,int(volcanoes_data(esp,ifoc))) *&! mass frac

!- only works for 1 volcano
      ash_size_dist(:)=ash_size_dist_data(:,int(volcanoes_data(esp,ifoc)))*0.01 !- fraction [0,1]
!      print*,'ash size dist=', int(volcanoes_data(esp,ifoc)),ash_size_dist(:)*100.
! tmp 
! P. Webley recommendation: 1.5e4 kg/s for a SO2 eruption rate
      QMSO2(ifoc)= 1.5e+4 * QDURA(ifoc)   ! kg for now
! tmp

!special setting for REDOULbT 1989
      ! QINJH(ifoc)=12.*1000.
      ! QDURA(ifoc)=40.*60.
      ! QMASH(ifoc)=(0.5*(0.058+0.105)) *1.e+9*0.27*particle_density

!-section for reading external values----------------------------------------
       if(use_these_values /= 'NONE'  .and. use_these_values /= 'none') then
          inquire(file=use_these_values,exist=there)
             if(.not.there) then
             print*,'file not found: ',trim(use_these_values)
             print*,'at get_emission_per_volcano routine'; stop
          endif
	  print*,"==> opening txt file=",trim(use_these_values)
          print*,'reading external values for INJ_HEIGHT, DURATION, MASS ASH, MASS SO2' 
          print*,'assuming units are meters (ASL) - seconds - kilograms - kilograms'
          OPEN (10,file=use_these_values,form='formatted')
          read(10,*) this_injh,this_dura,this_mash,this_so2
          print*,'assigned values are:',this_injh,this_dura,this_mash,this_so2
          print*,'reading external values ash size distribution (percentage)' 
          read(10,*) ash_size_dist(:)
	  close(10)
          print*,'assigned values are:',ash_size_dist(:)
          QINJH(ifoc)=this_injh - QELEV(ifoc) 
          QDURA(ifoc)=this_dura
          QMASH(ifoc)=this_mash
          QMSO2(ifoc)=this_so2
	  !converting to fraction
	  ash_size_dist(:)=ash_size_dist(:)*0.01
	  !print*,sum(ash_size_dist)
	  !if(int(sum(ash_size_dist)+1./100.) .ne. 1) stop 'sum(ash_size_dist) must be 1)'
     endif
!-----------------------------------------------------------------------------        
!---------------------------------------------------------------------
!not defined
      QBEGT(ifoc)=-999.
      QCRAT(ifoc)=-999.
      QHEAT(ifoc)=-999.
!not defined
!print*, ifoc ,QLONG(ifoc), QLATI(ifoc), QELEV(ifoc), QINJH(ifoc), QDURA(ifoc), QMASH(ifoc)
     
     
      do iesp=1,nspecies
       
       qsc(ifoc,iesp) = -9999.!not defined yet
       if(iesp==ash)qsc(ifoc,iesp) = QMASH(ifoc)
       if(iesp==so2)qsc(ifoc,iesp) = QMSO2(ifoc)
       
     enddo
!    print*,'ii,so2,ash=',ifoc,qsc(ifoc,so2),qsc(ifoc,ash)
!    call flush(6)
     
enddo

end subroutine get_emission_per_volcano
!---------------------------------------------------------------
subroutine interp_volc_to_model(ng,n1,n2,n3,xt,yt,xm,ym,deltax,deltay,plat,plon,rlat&
                         ,rlon,rland,nspecies_actual&
			 ,volcano_index,nvolcanoes  &
			 ,qsc,qlon,qlat,qelev,qBEGT,qcrat,qheat,qinjh &
                         ,qdura,QMASH,QMSO2)
			 
use grid_dims_out, only: grid_type
use mem_grid, only : grid_g
use volcanoes_emissions, only : volc_prop,volcanoes_g,volcanoesP_g,INJH,DURA,BEGT,ELEV
implicit none
integer, intent(in) ::volcano_index,nspecies_actual,nvolcanoes

integer, intent(in) :: ng,n1,n2,n3
integer :: igbox,jgbox
real deltax,deltay,plat,plon
real,    dimension(nvolcanoes) , intent(in):: &
               qlon,qlat,qelev,qBEGT&
	      ,qcrat,qheat,qinjh ,qdura  ,QMASH    ,QMSO2
real,    dimension(nvolcanoes,nspecies_actual), intent(in) :: qsc
real xt(n1),yt(n2),xm(n1),ym(n2)
real, dimension(n1,n2):: rlat,rlon,rland
real, allocatable, dimension(:,:):: grid_area

integer i1,i2,iesp,ifoc,i,j
real wlon,elon,xlon,splon

wlon=minval(rlon)   !tks
elon=maxval(rlon)   !tks

if(volcano_index==0) then 
 i1=1
 i2=nvolcanoes
else
 i1=volcano_index
 i2=volcano_index
endif

do ifoc = i1,i2

   if(grid_type == 'rams' .or. grid_type == 'polar') then
   	   call get_ij_rams(n1,n2,xt,yt,xm,ym,plat,plon,deltax,deltay&
   			   ,qlon(ifoc),qlat(ifoc),igbox,jgbox)
			   
   elseif(grid_type == 'lambert'.or. grid_type == 'mercator' ) then              
           xlon=qlon(ifoc)                          
           if(wlon.lt.-180)then
             splon=wlon+360.                        
             if(xlon.ge.splon)xlon=xlon-360.        
           endif
           if(elon.gt.180)then
             splon=elon-360.                        
             if(xlon.le.splon)xlon=xlon+360.        
           endif
  	   call get_ij_rams(n1,n2,xt,yt,xm,ym,plat,plon,deltax,deltay &
			   ,xlon,qlat(ifoc),igbox,jgbox)

   elseif(grid_type == 'll') then
   	   call get_ij_ll(n1,n2,rlat,rlon,qlon(ifoc),qlat(ifoc),igbox,jgbox)

   elseif(grid_type == 'fim') then
   	   call get_ij_fim(n1,n2,rlat,rlon,qlon(ifoc),qlat(ifoc),igbox,jgbox)

   elseif(grid_type == 'fv3') then
   	   call get_ij_fv3(n1,n2,rlat,rlon,qlon(ifoc),qlat(ifoc),igbox,jgbox)

   elseif(grid_type == 'gg') then
   	   call get_ij_gg(n1,n2,rlat,rlon,qlon(ifoc),qlat(ifoc),igbox,jgbox)
   else
            stop 'unknown grid type at volcanoes emissions '
   endif

   
   ! print*,'fire=',ifoc,igbox,jgbox,rlat(igbox,jgbox),rlon(igbox,jgbox)
   !print *,'ycit=',rlat(igbox,jgbox)
   !print *,'xcit=',rlon(igbox,jgbox)!,area(nprg),test  !lat,lon
   !print*,'dummy=main(xcit,ycit)' 


   if(igbox == -1 .or. jgbox == -1 ) cycle

   volcanoesP_g(INJH )%prop(igbox,jgbox) = volc_prop(INJH)%volcano(ifoc)
   volcanoesP_g(DURA )%prop(igbox,jgbox) = volc_prop(DURA)%volcano(ifoc)
   volcanoesP_g(BEGT )%prop(igbox,jgbox) = volc_prop(BEGT)%volcano(ifoc)
   volcanoesP_g(ELEV )%prop(igbox,jgbox) = volc_prop(ELEV)%volcano(ifoc)


   do iesp=1,nspecies_actual
!- total mass emitted  per gridbox
!     volcanoes_g(iesp)%src(igbox,jgbox,1)    = volcanoes_g(iesp)%src(igbox,jgbox,1) &
!                                              + qsc(ifoc,iesp)						      
! for now only 1 volcano is permited for each gid box
     volcanoes_g(iesp)%src(igbox,jgbox,1)    = &!volcanoes_g(iesp)%src(igbox,jgbox,1) &
                                              + qsc(ifoc,iesp)						      


!-  mass emitted (during the flaming phase) per gridbox 
!     if(use_bbem_plumerise /= 1) cycle
!     bbbem_plume_g(iesp,iveg_ag)%src(igbox,jgbox,1) = bbbem_plume_g(iesp,iveg_ag)%src(igbox,jgbox,1) &
!
!                                                    + qsc(ifoc,iesp)*flaming(iveg_ag)
   enddo
   
!   if(use_bbem_plumerise /= 1) cycle
!------------
!-fires properties : accumulated size    
!    bbbem_plume_fire_prop_g(qarea_agreg,iveg_ag)%fire_prop(igbox,jgbox)  =              &
!    bbbem_plume_fire_prop_g(qarea_agreg,iveg_ag)%fire_prop(igbox,jgbox)  + qsize (ifoc)
     
!-fires properties : fire number
!    bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(igbox,jgbox) =              &
!    bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(igbox,jgbox) + qfires (ifoc)

!  if(get_STD_of_size == 1 .and.   qsize (ifoc) > 0. .and. &
!     bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(igbox,jgbox) > 0.) &

!      qarea_I_agreg( igbox,jgbox,iveg_ag,&
!                     nint(bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(igbox,jgbox)))&
!		     =qsize (ifoc)
!------------
!
 write(*,*)'ifoc,igbox,jgbox,emis=',ifoc,igbox,jgbox,volcanoes_g(1)%src(igbox,jgbox,1) &
 ,volcanoes_g(2)%src(igbox,jgbox,1)
 call flush(6)
enddo

!
!if(use_bbem_plumerise == 1) then

!- produce some statistical quantities for the plumerise model
!do i=1,n1
!  do j=1,n2
!    do iveg_ag=1,nveg_agreg
! calculate the mean size of aggregate fire

!       if( bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(i,j) .ge. 1.) then 
          
!	  bbbem_plume_fire_prop_g(qarea_agreg ,iveg_ag)%fire_prop(i,j) = & ! mean  size =
!	  bbbem_plume_fire_prop_g(qarea_agreg ,iveg_ag)%fire_prop(i,j) / & ! total size / nfires
!          bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(i,j)
!      else   
!          bbbem_plume_fire_prop_g(qarea_agreg ,iveg_ag)%fire_prop(i,j) = 0.
!      endif
!! get the STD of fire size, if necessary
!      if (get_STD_of_size == 1) then
!      !  loop over all fires aggregate within a grid box (i,j)   => STD of size    
!        do ifoc=1,nint(bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(i,j))
!         
!orig    qarea_std_agreg(i,j,iveg_ag) = qarea_std_agreg(i,j,iveg_ag) + &
!orig                (qarea_I_agreg(i,j,iveg_ag,ifoc) - qarea_agreg(i,j,iveg_ag)) **2
	 
!	   bbbem_plume_fire_prop_g(qarea_std_agreg,iveg_ag)%fire_prop(i,j)  =  &
!	   bbbem_plume_fire_prop_g(qarea_std_agreg,iveg_ag)%fire_prop(i,j)  +  &
!           ( qarea_I_agreg(i,j,iveg_ag,ifoc)                                -  & 
!	     bbbem_plume_fire_prop_g(qarea_agreg ,iveg_ag)%fire_prop(i,j) )**2

!        enddo
!orig  qarea_std_agreg(i,j,iveg_ag) = sqrt(    qarea_std_agreg(i,j,iveg_ag)  / &
!orig                                        (1.e-4+qfires_agreg(i,j,iveg_ag)) )
!            bbbem_plume_fire_prop_g(qarea_std_agreg,iveg_ag)%fire_prop(i,j)  =  &
!       sqrt(bbbem_plume_fire_prop_g(qarea_std_agreg,iveg_ag)%fire_prop(i,j)) /  &
!            (1.e-4+bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(i,j))
!      endif
!
!- convert from mass consumed during the flaming phase to fraction of the total mass consumed
     
      
!      do iesp=1,nspecies_actual
!	 if(bbbem_g(iesp)%src(i,j,1) > 0.) &	      
	 
!orig      qsco_agreg  (i,j,iveg_ag) = qsco_agreg  (i,j,iveg_ag) /qsco (i,j)
!- fraction consumed at the flamin phase:
!           bbbem_plume_g(iesp,iveg_ag)%src(i,j,1) = bbbem_plume_g(iesp,iveg_ag)%src(i,j,1) &
!                                                  / bbbem_g(iesp)%src(i,j,1)
!      enddo
!    enddo 
! enddo
!enddo
!-new section

!don't change of place these lines below ...
!-------------- convert to surface flux (kg/m^2)
!- calculate the grib box area
allocate(grid_area(n1,n2))
if(grid_type == 'rams' .or. grid_type == 'polar') then
         call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)!

 elseif(grid_type == 'lambert' .or. grid_type == 'mercator' ) then
        grid_area(:,:)=1./(grid_g(ng)%dxt(:,:)*grid_g(ng)%dyt(:,:))
 
 elseif(grid_type == 'll') then
         call get_area_ll(grid_area,n1,n2) 

 elseif(grid_type == 'fim') then
         call get_area_fim(grid_area,n1,n2) 
 
 elseif(grid_type == 'fv3') then
           do j=1,n2
           do i=1,n1
           grid_area(i,j)=rland(i,j)
           enddo
           enddo
 
 elseif(grid_type == 'gg') then
         call get_area_gg(grid_area,n1,n2,rlat,rlon) 
	  
 else
       stop 'unknown grid type at volc emissions '
 endif

! convert from mass [kg/day] to flux [kg/m^2/day]
do iesp=1,nspecies_actual
      volcanoes_g(iesp)%src(:,:,1) = volcanoes_g(iesp)%src(:,:,1)/grid_area(:,:)
      print*,'volc=',iesp,maxval(volcanoes_g(iesp)%src(:,:,1))
enddo
!  
deallocate(grid_area)

end subroutine interp_volc_to_model
!---------------------------------------------------------------------------

subroutine get_volc_indentity(spc_name,ident)
!use chem1_list
use volcanoes_emissions, only : volcanoes_nspecies=>nspecies& 
                               ,volcanoes_spc_name=>spc_name
implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,volcanoes_nspecies
  ident=-1

!-special treatment for volcanic ASH :
!- bin 1 (v_ash1) will store the total ash, to be further splitted 
!- between the 10 bins using the size distribution fraction

!  if(spc_name == volcanoes_spc_name(isp) .or. spc_name =='V_'//trim(volcanoes_spc_name(isp))//'1') then
  if(spc_name == volcanoes_spc_name(isp)) then
      print*,'==>volcanoes found for ',spc_name,isp
      ident=isp
      return
   endif
enddo

end subroutine get_volc_indentity
!---------------------------------------------------------------------------

subroutine convert_volc_erup_to_aer(isp,iespc,ident,spc_name)
!use chem1_list
use emiss_vars_emissions
use volcanoes_emissions, only : volcanoes_nspecies=>nspecies& 
                               ,volcanoes_spc_name=>spc_name&
			       ,volcanoes_g&
			       ,ASH
implicit none
integer, intent(in) :: isp
integer, intent(inout) :: iespc,ident
character (len=*), intent(in)  :: spc_name   

!-- volc    |    aer 
if(spc_name == 'V_ASH1') then
   ident = ASH
   iespc=iespc+1
   emiss_spc_name(iespc,geoge)       = spc_name
   emiss_g(iespc)%src_geoge(:,:,1)   = volcanoes_g(ASH)%src(:,:,1)!*
   found_emiss_spc(iespc,geoge)    = 1
   print*,'==> converted from volc erup - found for ',spc_name
   return
endif

end subroutine convert_volc_erup_to_aer

