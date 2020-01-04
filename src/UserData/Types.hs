{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module UserData.Types where

import qualified Data.Aeson as J
import GHC.Generics (Generic)
import Data.Text (Text)

data Gender = Male | Female | Other deriving (Show, Generic)

data User = User
  { id_ :: !Int
  , first_name :: !Text
  , last_name :: !Text
  , email :: !Text
  , gender :: !Gender
  , num :: !Int
  , latitude :: !Double
  , longitude :: !Double
  } deriving (Show, Generic)

instance J.FromJSON Gender
instance J.ToJSON Gender

users :: [User]
users =
  [ User { id_        = 1
         , first_name = "Shane"
         , last_name  = "Plett"
         , email      = "splett0@free.fr"
         , gender     = Male
         , num        = -222
         , latitude   = 53.3928271
         , longitude  = 18.3836801
         }
  , User { id_        = 2
         , first_name = "Mata"
         , last_name  = "Snead"
         , email      = "msnead1@biblegateway.com"
         , gender     = Male
         , num        = -816
         , latitude   = 51.5141668
         , longitude  = -0.1331854
         }
  , User { id_        = 3
         , first_name = "Levon"
         , last_name  = "Sammes"
         , email      = "lsammes2@woothemes.com"
         , gender     = Male
         , num        = 485
         , latitude   = 51.6561
         , longitude  = 35.9314
         }
  , User { id_        = 4
         , first_name = "Irina"
         , last_name  = "Gourlay"
         , email      = "igourlay3@squarespace.com"
         , gender     = Female
         , num        = -434
         , latitude   = 40.4751843
         , longitude  = -8.6970168
         }
  , User { id_        = 5
         , first_name = "Brooks"
         , last_name  = "Titlow"
         , email      = "btitlow4@java.com"
         , gender     = Male
         , num        = 590
         , latitude   = 49.9800949
         , longitude  = 19.234704
         }
  , User { id_        = 6
         , first_name = "Antons"
         , last_name  = "Culleton"
         , email      = "aculleton5@google.com.hk"
         , gender     = Male
         , num        = -832
         , latitude   = 58.6840661
         , longitude  = 17.0829438
         }
  , User { id_        = 7
         , first_name = "Regine"
         , last_name  = "Emerton"
         , email      = "remerton6@mit.edu"
         , gender     = Female
         , num        = -846
         , latitude   = 32.5746598
         , longitude  = -117.0697958
         }
  , User { id_        = 8
         , first_name = "Starlin"
         , last_name  = "Laying"
         , email      = "slaying7@qq.com"
         , gender     = Female
         , num        = 544
         , latitude   = -7.8184706
         , longitude  = 110.3922413
         }
  , User { id_        = 9
         , first_name = "Orv"
         , last_name  = "Kempshall"
         , email      = "okempshall8@ebay.com"
         , gender     = Male
         , num        = -308
         , latitude   = 43.817071
         , longitude  = 125.323544
         }
  , User { id_        = 10
         , first_name = "Elizabeth"
         , last_name  = "Joseff"
         , email      = "ejoseff9@kickstarter.com"
         , gender     = Female
         , num        = 670
         , latitude   = -17.72332
         , longitude  = -149.3081197
         }
  , User { id_        = 11
         , first_name = "Cathee"
         , last_name  = "Eberz"
         , email      = "ceberza@hud.gov"
         , gender     = Female
         , num        = -801
         , latitude   = 16.7054663
         , longitude  = 121.672666
         }
  , User { id_        = 12
         , first_name = "Chiquia"
         , last_name  = "Font"
         , email      = "cfontb@seesaa.net"
         , gender     = Female
         , num        = -578
         , latitude   = 34.8281946
         , longitude  = 32.4070834
         }
  , User { id_        = 13
         , first_name = "Joycelin"
         , last_name  = "Jeste"
         , email      = "jjestec@creativecommons.org"
         , gender     = Female
         , num        = 18
         , latitude   = -8.2138062
         , longitude  = 114.3521951
         }
  , User { id_        = 14
         , first_name = "Leanna"
         , last_name  = "Alway"
         , email      = "lalwayd@google.com.br"
         , gender     = Female
         , num        = -577
         , latitude   = 61.1066573
         , longitude  = 21.4727058
         }
  , User { id_        = 15
         , first_name = "Zea"
         , last_name  = "Spradbrow"
         , email      = "zspradbrowe@unc.edu"
         , gender     = Female
         , num        = -779
         , latitude   = 13.5730766
         , longitude  = 123.0925437
         }
  , User { id_        = 16
         , first_name = "Wendell"
         , last_name  = "Jennions"
         , email      = "wjennionsf@arizona.edu"
         , gender     = Male
         , num        = -540
         , latitude   = 27.615202
         , longitude  = 113.870729
         }
  , User { id_        = 17
         , first_name = "Akim"
         , last_name  = "Fernely"
         , email      = "afernelyg@prnewswire.com"
         , gender     = Male
         , num        = -553
         , latitude   = 38.7726507
         , longitude  = -7.502314
         }
  , User { id_        = 18
         , first_name = "Dina"
         , last_name  = "Morad"
         , email      = "dmoradh@stanford.edu"
         , gender     = Female
         , num        = -983
         , latitude   = 52.6225919
         , longitude  = 90.0794098
         }
  , User { id_        = 19
         , first_name = "Clarette"
         , last_name  = "Lennarde"
         , email      = "clennardei@pcworld.com"
         , gender     = Female
         , num        = -138
         , latitude   = 31.236833
         , longitude  = 121.532286
         }
  , User { id_        = 20
         , first_name = "Letitia"
         , last_name  = "Tredger"
         , email      = "ltredgerj@furl.net"
         , gender     = Female
         , num        = -70
         , latitude   = -8.5694
         , longitude  = 116.1325
         }
  , User { id_        = 21
         , first_name = "Jackquelin"
         , last_name  = "Donnan"
         , email      = "jdonnank@sourceforge.net"
         , gender     = Female
         , num        = 922
         , latitude   = 38.984289
         , longitude  = 118.498758
         }
  , User { id_        = 22
         , first_name = "Leland"
         , last_name  = "Nehl"
         , email      = "lnehll@goo.gl"
         , gender     = Male
         , num        = -410
         , latitude   = 13.3601931
         , longitude  = 99.9129845
         }
  , User { id_        = 23
         , first_name = "Noe"
         , last_name  = "Ajsik"
         , email      = "najsikm@psu.edu"
         , gender     = Male
         , num        = 527
         , latitude   = 50.6927144
         , longitude  = 16.7525932
         }
  , User { id_        = 24
         , first_name = "Emlen"
         , last_name  = "Circuitt"
         , email      = "ecircuittn@go.com"
         , gender     = Male
         , num        = 117
         , latitude   = 35.0475045
         , longitude  = 136.1219714
         }
  , User { id_        = 25
         , first_name = "Dov"
         , last_name  = "Passe"
         , email      = "dpasseo@reuters.com"
         , gender     = Male
         , num        = -713
         , latitude   = 12.8207931
         , longitude  = -84.2002101
         }
  , User { id_        = 26
         , first_name = "Winfield"
         , last_name  = "Kells"
         , email      = "wkellsp@netvibes.com"
         , gender     = Male
         , num        = 92
         , latitude   = 59.1102039
         , longitude  = 9.6759643
         }
  , User { id_        = 27
         , first_name = "Udall"
         , last_name  = "Mac Giany"
         , email      = "umacgianyq@cocolog-nifty.com"
         , gender     = Male
         , num        = 655
         , latitude   = 8.3412493
         , longitude  = -77.2175223
         }
  , User { id_        = 28
         , first_name = "Curcio"
         , last_name  = "Dimmick"
         , email      = "cdimmickr@tripod.com"
         , gender     = Male
         , num        = 650
         , latitude   = 47.3591366
         , longitude  = 2.8002946
         }
  , User { id_        = 29
         , first_name = "Linzy"
         , last_name  = "Kemp"
         , email      = "lkemps@guardian.co.uk"
         , gender     = Female
         , num        = 131
         , latitude   = 35.38808
         , longitude  = 36.13439
         }
  , User { id_        = 30
         , first_name = "Ellery"
         , last_name  = "Vince"
         , email      = "evincet@cyberchimps.com"
         , gender     = Male
         , num        = 821
         , latitude   = -16.4981761
         , longitude  = -69.1014935
         }
  , User { id_        = 31
         , first_name = "Rodney"
         , last_name  = "Spence"
         , email      = "rspenceu@indiegogo.com"
         , gender     = Male
         , num        = 499
         , latitude   = -8.1830575
         , longitude  = 113.9358031
         }
  , User { id_        = 32
         , first_name = "Ree"
         , last_name  = "Minker"
         , email      = "rminkerv@storify.com"
         , gender     = Female
         , num        = -999
         , latitude   = 7.55633
         , longitude  = 99.61141
         }
  , User { id_        = 33
         , first_name = "Saxe"
         , last_name  = "Assante"
         , email      = "sassantew@goo.gl"
         , gender     = Male
         , num        = -420
         , latitude   = 7.1270158
         , longitude  = 125.8973301
         }
  , User { id_        = 34
         , first_name = "Filberte"
         , last_name  = "Yurinov"
         , email      = "fyurinovx@squarespace.com"
         , gender     = Male
         , num        = -88
         , latitude   = -9.3294895
         , longitude  = 125.2697802
         }
  , User { id_        = 35
         , first_name = "Toinette"
         , last_name  = "Sidgwick"
         , email      = "tsidgwicky@cargocollective.com"
         , gender     = Female
         , num        = 636
         , latitude   = 14.0412181
         , longitude  = 122.9421882
         }
  , User { id_        = 36
         , first_name = "Derick"
         , last_name  = "Bouttell"
         , email      = "dbouttellz@sourceforge.net"
         , gender     = Male
         , num        = -987
         , latitude   = -6.2194518
         , longitude  = 107.2991511
         }
  , User { id_        = 37
         , first_name = "Derril"
         , last_name  = "O'Sheeryne"
         , email      = "dosheeryne10@unc.edu"
         , gender     = Male
         , num        = 50
         , latitude   = 47.8840341
         , longitude  = 29.3465596
         }
  , User { id_        = 38
         , first_name = "Trstram"
         , last_name  = "Keyme"
         , email      = "tkeyme11@chron.com"
         , gender     = Male
         , num        = -456
         , latitude   = -7.9017349
         , longitude  = 112.5632485
         }
  , User { id_        = 39
         , first_name = "Myrtice"
         , last_name  = "Lynnitt"
         , email      = "mlynnitt12@blinklist.com"
         , gender     = Female
         , num        = 946
         , latitude   = 50.50505
         , longitude  = 16.74043
         }
  , User { id_        = 40
         , first_name = "Ned"
         , last_name  = "Jeaves"
         , email      = "njeaves13@wiley.com"
         , gender     = Male
         , num        = 663
         , latitude   = 49.1656853
         , longitude  = 16.7861155
         }
  , User { id_        = 41
         , first_name = "Thorny"
         , last_name  = "Troke"
         , email      = "ttroke14@google.com"
         , gender     = Male
         , num        = 594
         , latitude   = 37.4749019
         , longitude  = 70.6142436
         }
  , User { id_        = 42
         , first_name = "Daveen"
         , last_name  = "Stevenson"
         , email      = "dstevenson15@youtube.com"
         , gender     = Female
         , num        = -485
         , latitude   = -30.5454936
         , longitude  = -52.5257016
         }
  , User { id_        = 43
         , first_name = "Mata"
         , last_name  = "Jellard"
         , email      = "mjellard16@adobe.com"
         , gender     = Male
         , num        = -339
         , latitude   = 15.2113128
         , longitude  = 120.0278872
         }
  , User { id_        = 44
         , first_name = "Lyn"
         , last_name  = "Callicott"
         , email      = "lcallicott17@redcross.org"
         , gender     = Male
         , num        = 211
         , latitude   = -4.7695926
         , longitude  = -40.5112755
         }
  , User { id_        = 45
         , first_name = "Carie"
         , last_name  = "Sam"
         , email      = "csam18@privacy.gov.au"
         , gender     = Female
         , num        = 959
         , latitude   = 42.7751413
         , longitude  = 59.6122792
         }
  , User { id_        = 46
         , first_name = "Corine"
         , last_name  = "Duffet"
         , email      = "cduffet19@google.pl"
         , gender     = Female
         , num        = 271
         , latitude   = 38.1481953
         , longitude  = 22.3558193
         }
  , User { id_        = 47
         , first_name = "Patience"
         , last_name  = "Pinnere"
         , email      = "ppinnere1a@abc.net.au"
         , gender     = Female
         , num        = 749
         , latitude   = 14.514787
         , longitude  = -87.3996765
         }
  , User { id_        = 48
         , first_name = "Pattin"
         , last_name  = "Blackborough"
         , email      = "pblackborough1b@japanpost.jp"
         , gender     = Male
         , num        = 603
         , latitude   = 5.4459387
         , longitude  = 10.0471549
         }
  , User { id_        = 49
         , first_name = "Hyacintha"
         , last_name  = "McCorrie"
         , email      = "hmccorrie1c@over-blog.com"
         , gender     = Female
         , num        = -237
         , latitude   = -0.6311749
         , longitude  = 100.1192169
         }
  , User { id_        = 50
         , first_name = "Parnell"
         , last_name  = "Kellaway"
         , email      = "pkellaway1d@craigslist.org"
         , gender     = Male
         , num        = -623
         , latitude   = 49.7514502
         , longitude  = 6.4641645
         }
  , User { id_        = 51
         , first_name = "Land"
         , last_name  = "Morfey"
         , email      = "lmorfey1e@toplist.cz"
         , gender     = Male
         , num        = 301
         , latitude   = -7.9821907
         , longitude  = -38.2893787
         }
  , User { id_        = 52
         , first_name = "Georgi"
         , last_name  = "Duny"
         , email      = "gduny1f@theglobeandmail.com"
         , gender     = Male
         , num        = -841
         , latitude   = 39.068035
         , longitude  = 20.6971047
         }
  , User { id_        = 53
         , first_name = "Jon"
         , last_name  = "Coare"
         , email      = "jcoare1g@booking.com"
         , gender     = Male
         , num        = -382
         , latitude   = 46.9067373
         , longitude  = 19.691788
         }
  , User { id_        = 54
         , first_name = "Priscilla"
         , last_name  = "Sharratt"
         , email      = "psharratt1h@behance.net"
         , gender     = Female
         , num        = -29
         , latitude   = 43.8322875
         , longitude  = 25.9534752
         }
  , User { id_        = 55
         , first_name = "Sunny"
         , last_name  = "Delamere"
         , email      = "sdelamere1i@goo.ne.jp"
         , gender     = Male
         , num        = -625
         , latitude   = 14.6333181
         , longitude  = -90.6703399
         }
  , User { id_        = 56
         , first_name = "Oralee"
         , last_name  = "Reader"
         , email      = "oreader1j@indiegogo.com"
         , gender     = Female
         , num        = 743
         , latitude   = -36.9532166
         , longitude  = -73.017378
         }
  , User { id_        = 57
         , first_name = "Shalne"
         , last_name  = "Gadman"
         , email      = "sgadman1k@spiegel.de"
         , gender     = Female
         , num        = 517
         , latitude   = -7.7474
         , longitude  = 114.0409
         }
  , User { id_        = 58
         , first_name = "Flori"
         , last_name  = "Gisborne"
         , email      = "fgisborne1l@g.co"
         , gender     = Female
         , num        = 631
         , latitude   = 28.3079856
         , longitude  = 117.697041
         }
  , User { id_        = 59
         , first_name = "Glynn"
         , last_name  = "Trigg"
         , email      = "gtrigg1m@economist.com"
         , gender     = Male
         , num        = -193
         , latitude   = 51.8321777
         , longitude  = 85.7788422
         }
  , User { id_        = 60
         , first_name = "Miranda"
         , last_name  = "Peppett"
         , email      = "mpeppett1n@shinystat.com"
         , gender     = Female
         , num        = 54
         , latitude   = -6.0189936
         , longitude  = -38.7368187
         }
  , User { id_        = 61
         , first_name = "Rochette"
         , last_name  = "O'Lennachain"
         , email      = "rolennachain1o@bluehost.com"
         , gender     = Female
         , num        = -877
         , latitude   = 28.615201
         , longitude  = 89.493117
         }
  , User { id_        = 62
         , first_name = "Madelle"
         , last_name  = "Kalker"
         , email      = "mkalker1p@hibu.com"
         , gender     = Female
         , num        = -73
         , latitude   = 43.7755615
         , longitude  = 23.7246154
         }
  , User { id_        = 63
         , first_name = "Quill"
         , last_name  = "Hawick"
         , email      = "qhawick1q@tiny.cc"
         , gender     = Male
         , num        = -381
         , latitude   = 37.5092473
         , longitude  = 140.9723283
         }
  , User { id_        = 64
         , first_name = "Cos"
         , last_name  = "Haughin"
         , email      = "chaughin1r@bloglovin.com"
         , gender     = Male
         , num        = -63
         , latitude   = 29.6252177
         , longitude  = -95.2208569
         }
  , User { id_        = 65
         , first_name = "Leoine"
         , last_name  = "Hellin"
         , email      = "lhellin1s@kickstarter.com"
         , gender     = Female
         , num        = 343
         , latitude   = 45.304073
         , longitude  = 130.980894
         }
  , User { id_        = 66
         , first_name = "Lotte"
         , last_name  = "MacDermand"
         , email      = "lmacdermand1t@tinyurl.com"
         , gender     = Female
         , num        = 754
         , latitude   = 43.1557012
         , longitude  = 22.5856811
         }
  , User { id_        = 67
         , first_name = "Raine"
         , last_name  = "Twinn"
         , email      = "rtwinn1u@apache.org"
         , gender     = Female
         , num        = 88
         , latitude   = -21.7112092
         , longitude  = 165.8299327
         }
  , User { id_        = 68
         , first_name = "Sherwin"
         , last_name  = "Lynas"
         , email      = "slynas1v@multiply.com"
         , gender     = Male
         , num        = -360
         , latitude   = 26.443249
         , longitude  = 107.253334
         }
  , User { id_        = 69
         , first_name = "Jerome"
         , last_name  = "Gourdon"
         , email      = "jgourdon1w@omniture.com"
         , gender     = Male
         , num        = -188
         , latitude   = -6.8083456
         , longitude  = 105.6735131
         }
  , User { id_        = 70
         , first_name = "Gillian"
         , last_name  = "Dradey"
         , email      = "gdradey1x@guardian.co.uk"
         , gender     = Female
         , num        = -173
         , latitude   = 51.5363109
         , longitude  = 24.8942909
         }
  , User { id_        = 71
         , first_name = "Philbert"
         , last_name  = "Cutcliffe"
         , email      = "pcutcliffe1y@msn.com"
         , gender     = Male
         , num        = 68
         , latitude   = 22.948016
         , longitude  = 113.366904
         }
  , User { id_        = 72
         , first_name = "Sheffield"
         , last_name  = "Gurnay"
         , email      = "sgurnay1z@webs.com"
         , gender     = Male
         , num        = 186
         , latitude   = 6.8117856
         , longitude  = 101.1843887
         }
  , User { id_        = 73
         , first_name = "Margareta"
         , last_name  = "Killingsworth"
         , email      = "mkillingsworth20@nhs.uk"
         , gender     = Female
         , num        = -544
         , latitude   = 53.8446847
         , longitude  = 18.9605242
         }
  , User { id_        = 74
         , first_name = "Meryl"
         , last_name  = "Moran"
         , email      = "mmoran21@meetup.com"
         , gender     = Male
         , num        = -645
         , latitude   = 39.0277832
         , longitude  = -94.6557914
         }
  , User { id_        = 75
         , first_name = "Opalina"
         , last_name  = "Aikin"
         , email      = "oaikin22@yolasite.com"
         , gender     = Female
         , num        = -216
         , latitude   = 47.3752386
         , longitude  = -0.8453268
         }
  , User { id_        = 76
         , first_name = "Marika"
         , last_name  = "Hodgins"
         , email      = "mhodgins23@prweb.com"
         , gender     = Female
         , num        = -992
         , latitude   = 40.9981633
         , longitude  = 47.8699826
         }
  , User { id_        = 77
         , first_name = "Robert"
         , last_name  = "Manske"
         , email      = "rmanske24@illinois.edu"
         , gender     = Male
         , num        = -233
         , latitude   = 48.8478808
         , longitude  = 2.5525914
         }
  , User { id_        = 78
         , first_name = "Killie"
         , last_name  = "Freebury"
         , email      = "kfreebury25@cisco.com"
         , gender     = Male
         , num        = -175
         , latitude   = 14.2157645
         , longitude  = 109.1166566
         }
  , User { id_        = 79
         , first_name = "Hattie"
         , last_name  = "Januszkiewicz"
         , email      = "hjanuszkiewicz26@scientificamerican.com"
         , gender     = Female
         , num        = 252
         , latitude   = 9.316446
         , longitude  = 16.1610343
         }
  , User { id_        = 80
         , first_name = "Chandal"
         , last_name  = "Gersam"
         , email      = "cgersam27@shinystat.com"
         , gender     = Female
         , num        = 697
         , latitude   = 40.5529581
         , longitude  = 44.9105417
         }
  , User { id_        = 81
         , first_name = "June"
         , last_name  = "Feitosa"
         , email      = "jfeitosa28@ow.ly"
         , gender     = Female
         , num        = 325
         , latitude   = 48.627474
         , longitude  = 2.5921846
         }
  , User { id_        = 82
         , first_name = "Francesco"
         , last_name  = "Hurdwell"
         , email      = "fhurdwell29@nature.com"
         , gender     = Male
         , num        = -340
         , latitude   = 29.1082924
         , longitude  = 119.6364468
         }
  , User { id_        = 83
         , first_name = "Giustino"
         , last_name  = "Cornforth"
         , email      = "gcornforth2a@thetimes.co.uk"
         , gender     = Male
         , num        = 284
         , latitude   = 47.015529
         , longitude  = -68.1429749
         }
  , User { id_        = 84
         , first_name = "Tam"
         , last_name  = "Swapp"
         , email      = "tswapp2b@noaa.gov"
         , gender     = Male
         , num        = -657
         , latitude   = 39.202804
         , longitude  = 113.117269
         }
  , User { id_        = 85
         , first_name = "Valentine"
         , last_name  = "Heggs"
         , email      = "vheggs2c@elpais.com"
         , gender     = Male
         , num        = 792
         , latitude   = 40.862786
         , longitude  = -8.4731932
         }
  , User { id_        = 86
         , first_name = "Worth"
         , last_name  = "Lochet"
         , email      = "wlochet2d@ted.com"
         , gender     = Male
         , num        = -521
         , latitude   = 29.7905037
         , longitude  = 72.1747127
         }
  , User { id_        = 87
         , first_name = "Morey"
         , last_name  = "Glidder"
         , email      = "mglidder2e@blogger.com"
         , gender     = Male
         , num        = -803
         , latitude   = 36.57863
         , longitude  = 114.114634
         }
  , User { id_        = 88
         , first_name = "Sasha"
         , last_name  = "Bramo"
         , email      = "sbramo2f@ow.ly"
         , gender     = Male
         , num        = 249
         , latitude   = 55.6789638
         , longitude  = 12.5383906
         }
  , User { id_        = 89
         , first_name = "Colet"
         , last_name  = "Kilmister"
         , email      = "ckilmister2g@engadget.com"
         , gender     = Male
         , num        = -949
         , latitude   = 32.661925
         , longitude  = 120.601922
         }
  , User { id_        = 90
         , first_name = "Lancelot"
         , last_name  = "Girardez"
         , email      = "lgirardez2h@weather.com"
         , gender     = Male
         , num        = 456
         , latitude   = 51.9301973
         , longitude  = 19.4450042
         }
  , User { id_        = 91
         , first_name = "Marijn"
         , last_name  = "Sumption"
         , email      = "msumption2i@zimbio.com"
         , gender     = Male
         , num        = 572
         , latitude   = -8.7517433
         , longitude  = 122.0183914
         }
  , User { id_        = 92
         , first_name = "Marina"
         , last_name  = "Cescoti"
         , email      = "mcescoti2j@yale.edu"
         , gender     = Female
         , num        = 390
         , latitude   = -22.7106685
         , longitude  = -43.5522256
         }
  , User { id_        = 93
         , first_name = "Randell"
         , last_name  = "Gush"
         , email      = "rgush2k@nyu.edu"
         , gender     = Male
         , num        = 22
         , latitude   = 34.41096
         , longitude  = 36.75879
         }
  , User { id_        = 94
         , first_name = "Teresina"
         , last_name  = "Seys"
         , email      = "tseys2l@wikia.com"
         , gender     = Female
         , num        = 630
         , latitude   = 7.9076232
         , longitude  = -80.4256976
         }
  , User { id_        = 95
         , first_name = "Jodie"
         , last_name  = "Hearnshaw"
         , email      = "jhearnshaw2m@angelfire.com"
         , gender     = Male
         , num        = -850
         , latitude   = 26.694208
         , longitude  = 115.939083
         }
  , User { id_        = 96
         , first_name = "Babbette"
         , last_name  = "Rock"
         , email      = "brock2n@xrea.com"
         , gender     = Female
         , num        = -125
         , latitude   = 18.5769276
         , longitude  = 100.2151578
         }
  , User { id_        = 97
         , first_name = "Aurel"
         , last_name  = "Wisson"
         , email      = "awisson2o@hhs.gov"
         , gender     = Female
         , num        = -625
         , latitude   = 26.641315
         , longitude  = 100.222545
         }
  , User { id_        = 98
         , first_name = "Martin"
         , last_name  = "Mattielli"
         , email      = "mmattielli2p@oakley.com"
         , gender     = Male
         , num        = 170
         , latitude   = 48.8466523
         , longitude  = 2.2582125
         }
  , User { id_        = 99
         , first_name = "Jeffrey"
         , last_name  = "Livezley"
         , email      = "jlivezley2q@wordpress.com"
         , gender     = Male
         , num        = 724
         , latitude   = 43.817071
         , longitude  = 125.323544
         }
  , User { id_        = 100
         , first_name = "Rafaelita"
         , last_name  = "Crotty"
         , email      = "rcrotty2r@google.com.au"
         , gender     = Female
         , num        = -73
         , latitude   = 36.13848
         , longitude  = 36.61247
         }
  ]
