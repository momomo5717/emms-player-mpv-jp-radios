;;; emms-streams-simul.el --- emms stream list for SimulRadio -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by

;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides emms stream list for SimulRadio.

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-simul-hokkaido
  '(("三角山放送局 : 札幌市西区 : 北海道" "simul://http://wm.sankakuyama.co.jp/asx/sankaku_24k.asx" 1 streamlist)
    ("FM JAGA : 帯広市 : 北海道" "simul://http://www.simulradio.info/asx/fmjaga.asx" 1 streamlist)
    ("FM WING : 帯広市 : 北海道" "simul://http://www.simulradio.info/asx/fmwing.asx" 1 streamlist)
    ("RadioD FM dramacity : 札幌市厚別区 : 北海道" "simul://http://dramacity.jp/fmdorama_24k.asx" 1 streamlist)
    ("FMくしろ : 釧路市 : 北海道" "simul://http://www.simulradio.info/asx/FmKushiro.asx" 1 streamlist)
    ("FMわっぴ～ : 稚内市 : 北海道" "simul://http://wappy761.jp/fmwappy.asx" 1 streamlist)
    ("FMりべーる : 旭川市 : 北海道" "simul://http://www.simulradio.info/asx/fm837.asx" 1 streamlist)
    ("ラジオニセコ : ニセコ町 : 北海道" "simul://http://www.radioniseko.jp/asx/radioniseko_24k.asx" 1 streamlist)
    ("FMいるか : 函館市 : 北海道" "simul://http://www.simulradio.info/asx/iruka.asx" 1 streamlist)
    ("ラジオカロスサッポロ : 札幌市 : 北海道" "simul://http://www.simulradio.info/asx/radiokaros.asx" 1 streamlist)
    ("FMアップル : 札幌市豊平区 : 北海道" "simul://http://www.simulradio.info/asx/fmapple.asx" 1 streamlist)
    ("e-niwaFM : 恵庭市 : 北海道" "simul://http://www.simulradio.info/asx/eniwa.asx" 1 streamlist))
  "Stream list of hokkaido area.")

(defvar emms-stream-simul-touhoku
  '(("ラヂオもりおか : 盛岡市 : 東北 : 災害対応局" "simul://http://www.simulradio.info/asx/radiomorioka.asx" 1 streamlist)
    ("RADIO3 : 仙台市青葉区 : 東北 : 災害対応局" "simul://http://www.simulradio.info/asx/radio3.asx" 1 streamlist)
    ("エフエム モットコム : 本宮市 : 東北 : 災害対応局" "simul://http://www.simulradio.info/asx/fmmotcom.asx" 1 streamlist)
    ("FMいわき : いわき市 : 東北 : 災害対応局" "simul://http://www.simulradio.info/asx/fm-iwaki.asx" 1 streamlist)
    ("エフエム会津 : 会津若松市 : 東北" "simul://http://www.simulradio.info/asx/aizu.asx" 1 streamlist)
    ("FMゆーとぴあ : 湯沢市 : 東北" "simul://http://www.simulradio.info/asx/FmYutopia.asx" 1 streamlist)
    ("横手かまくらエフエム : 横手市 : 東北" "simul://http://www.simulradio.info/asx/yokote.asx" 1 streamlist)
    ("みやこハーバーラジオ : 宮古市 : 東北 : 災害対応局" "simul://http://www.simulradio.info/asx/FmMiyako.asx" 1 streamlist)
    ("ラジオ石巻 : 石巻市 : 東北" "simul://http://www.simulradio.info/asx/RadioIshinomaki.asx" 1 streamlist)
    ("BAY WAVE : 塩釜市 : 東北" "simul://http://www.simulradio.info/asx/BAYWAVE.asx" 1 streamlist)
    ("fmいずみ : 仙台市泉区 : 東北 : 災害対応局" "simul://http://www.simulradio.info/asx/fmIzumi.asx" 1 streamlist)
    ("りんごFM : 山元町 : 東北" "simul://http://www.simulradio.info/asx/RingoFM.asx" 1 streamlist)
    ("なとらじ : 名取市 : 東北" "simul://http://www.simulradio.info/asx/Natoraji.asx" 1 streamlist)
    ("南相馬ひばりエフエム : 南相馬市 : 東北" "simul://http://www.simulradio.info/asx/MinamisomaFM.asx" 1 streamlist)
    ("郡山コミュニティ放送 : 郡山市 : 東北 : 災害対応局" "simul://http://www.simulradio.info/asx/kocofm.asx" 1 streamlist)
    ("女川さいがいFM : 女川町 : 東北" "simul://http://www.simulradio.info/asx/OnagawaFM.asx" 1 streamlist)
    ("けせんぬまさいがいエフエム : 気仙沼市 : 東北" "simul://http://www.simulradio.info/asx/kesennumaFM.asx" 1 streamlist)
    ("陸前高田災害FM : 陸前高田市 : 東北" "simul://http://www.simulradio.info/asx/rikuzentakataFM.asx" 1 streamlist)
    ("富岡臨時災害FM局（おだがいさまFM） : 富岡町 : 東北" "simul://http://www.simulradio.info/asx/OdagaisamaFM.asx" 1 streamlist)
    ("亘理臨時災害FM局（FMあおぞら） : 亘理町 : 東北" "simul://http://www.simulradio.info/asx/aozora.asx" 1 streamlist)
    ("FMねまらいん : 大船渡市 : 東北" "simul://mms://hdv.nkansai.tv/ofunato" 1 streamlist)
    ("おおつちさいがいエフエム : 大槌町 : 東北" "simul://http://www.simulradio.info/asx/otsuchi.asx" 1 streamlist)
    ("釜石災害FM : 釜石市 : 東北" "simul://http://www.simulradio.info/asx/kamaishi.asx" 1 streamlist)
    ("FMあすも : 一関市 : 東北" "simul://http://fmasmo.fmplapla.com/player/" 1 streamlist)
    ("BeFM : 八戸市 : 東北" "simul://http://www.simulradio.info/asx/befm.asx" 1 streamlist)
    ("鹿角きりたんぽFM : 鹿角市 : 東北" "simul://http://www.simulradio.info/asx/kiritampo.asx" 1 streamlist))
  "Stream list of touhoku area.")

(defvar emms-stream-simul-shinetsu
  '(("FM Kento : 新潟市中央区 : 信越" "simul://http://www.simulradio.info/asx/fmkento.asx" 1 streamlist)
    ("FM軽井沢 : 軽井沢町 : 信越" "simul://http://www.simulradio.info/asx/fmkaruizawa.asx" 1 streamlist)
    ("FMさくだいら : 佐久市 : 信越" "simul://http://www.simulradio.info/asx/sakudaira.asx" 1 streamlist)
    ("あづみ野FM : 安曇野市 : 信越" "simul://http://www.simulradio.info/asx/azumino.asx" 1 streamlist))
  "Stream list of shinetsu area.")

(defvar emms-stream-simul-kantou
  '(("FMぱるるん : 水戸市 : 関東 : 災害対応局" "simul://http://www.simulradio.info/asx/fmpalulun.asx" 1 streamlist)
    ("フラワーラジオ : 鴻巣市 : 関東" "simul://http://www.fm767.com/flower_64k.asx" 1 streamlist)
    ("すまいるFM : 朝霞市 : 関東" "simul://http://www.simulradio.info/asx/smile.asx" 1 streamlist)
    ("湘南ビーチFM : 逗子市・葉山町 : 関東" "simul://http://www.simulradio.info/asx/shonanbeachfm.asx" 1 streamlist)
    ("レディオ湘南 : 藤沢市 : 関東" "simul://http://www.simulradio.info/asx/radioshonan.asx" 1 streamlist)
    ("FMおだわら : 小田原市 : 関東" "simul://http://www.simulradio.info/asx/fmodawara.asx" 1 streamlist)
    ("REDS WAVE : さいたま市 : 関東" "simul://http://redswave.com/simul.asx" 1 streamlist)
    ("ラヂオつくば : つくば市 : 関東 : 災害対応局" "simul://http://www.simulradio.info/asx/tsukuba.asx" 1 streamlist)
    ("エフエムたちかわ : 立川市 : 関東" "simul://http://www.simulradio.info/asx/fm-tachikawa.asx" 1 streamlist)
    ("かわさきFM : 川崎市 : 関東" "simul://http://www.simulradio.info/asx/kawasaki.asx" 1 streamlist)
    ("FM 桐生 : 桐生市 : 関東" "simul://http://www.simulradio.info/asx/kiryufm.asx" 1 streamlist)
    ("FMやまと : 大和市 : 関東" "simul://http://www.simulradio.info/asx/FmYamato.asx" 1 streamlist)
    ("FM戸塚 : 横浜市 : 関東" "simul://http://www.simulradio.info/asx/totsuka.asx" 1 streamlist)
    ("FMサルース : 横浜市 : 関東" "simul://http://www.simulradio.info/asx/FmSalus.asx" 1 streamlist)
    ("調布FM : 調布市 : 関東" "simul://http://www.simulradio.info/asx/chofu_fm.asx" 1 streamlist)
    ("まえばしCITYエフエム : 前橋市 : 関東" "simul://http://radio.maebashi.fm:8080/mwave" 1 streamlist)
    ("かつしかFM : 葛飾区 : 関東" "simul://http://www.simulradio.info/asx/katsushika.asx" 1 streamlist)
    ("エフエムさがみ : 相模原市 : 関東" "simul://http://www.fmsagami.co.jp/asx/fmsagami.asx" 1 streamlist)
    ("レインボータウンFM : 江東区 : 関東" "simul://http://www.simulradio.info/asx/rainbowtown.asx" 1 streamlist)
    ("FM kaon : 海老名市 : 関東" "simul://mms://hdv.nkansai.tv/kaon" 1 streamlist)
    ("中央エフエム : 中央区 : 関東" "simul://http://www.simulradio.info/asx/chuo_fm.asx" 1 streamlist)
    ("たかはぎFM : 高萩市 : 関東" "simul://http://www.simulradio.info/asx/takahagi.asx" 1 streamlist) )
  "Stream list of kantou area.")

(defvar emms-stream-simul-toukai
  '(("PORT WAVE : 四日市市 : 東海" "simul://http://www.simulradio.info/asx/portwavefm.asx" 1 streamlist)
    ("Ciao! : 熱海市 : 東海" "simul://http://www.simulradio.info/asx/ciao.asx" 1 streamlist)
    ("MID-FM : 名古屋市中区 : 東海" "simul://http://www.simulradio.info/asx/mid-fm761.asx" 1 streamlist)
    ("FMおかざき : 岡崎市 : 東海" "simul://http://www.simulradio.info/asx/FmOkazaki.asx" 1 streamlist)
    ("Pitch FM : 刈谷市 : 東海" "simul://http://www.simulradio.info/asx/pitch.asx" 1 streamlist)
    ("RADIO LOVEAT : 豊田市 : 東海" "simul://http://www.simulradio.info/asx/toyota.asx" 1 streamlist)
    ("Suzuka Voice FM : 鈴鹿市 : 東海" "simul://http://www.simulradio.info/asx/suzuka.asx" 1 streamlist)
    ("FMいずのくに : 伊豆の国市 : 東海" "simul://http://www.simulradio.info/asx/izunokuni.asx" 1 streamlist))
  "Stream list of toukai area.")

(defvar emms-stream-simul-hokuriku
  '(("FM-N1 : 野々市市 : 北陸" "simul://http://android.fmn1.jp/live/" 1 streamlist)
    ("ハーバーステーション : 敦賀市 : 北陸" "simul://http://www.web-services.jp/harbor779/" 1 streamlist)
    ("ラジオ・ミュー : 黒部市 : 北陸" "simul://http://www.simulradio.info/asx/radiomyu.asx" 1 streamlist))
  "Stream list of hokuriku area.")

(defvar emms-stream-simul-kinki
  '(("FM丹波 : 福知山市 : 近畿" "simul://http://fukuchiyama.fm-tanba.jp/simul.asx" 1 streamlist)
    ("FM 千里 : 豊中市 : 近畿" "simul://http://www.simulradio.info/asx/fmsenri.asx" 1 streamlist)
    ("エフエムわいわい : 神戸市 : 近畿" "simul://http://www.simulradio.info/asx/fmyy.asx" 1 streamlist)
    ("FM HANAKO : 守口市 : 近畿" "simul://http://fmhanako.jp/radio/824.asx" 1 streamlist)
    ("エフエム　みっきぃ : 三木市 : 近畿" "simul://http://www.simulradio.info/asx/fm-miki.asx" 1 streamlist)
    ("FMひらかた : 枚方市 : 近畿" "simul://http://www.simulradio.info/asx/hirakata.asx" 1 streamlist)
    ("FM GENKI : 姫路市 : 近畿" "simul://http://www.simulradio.info/asx/fm-genki.asx" 1 streamlist)
    ("FM TANABE : 田辺市 : 近畿" "simul://http://www.simulradio.info/asx/fm-tanabe.asx" 1 streamlist)
    ("FMジャングル : 豊岡市 : 近畿" "simul://http://www.simulradio.info/asx/jungle.asx" 1 streamlist)
    ("BAN-BANラジオ : 加古川市 : 近畿" "simul://http://www.simulradio.info/asx/banban.asx" 1 streamlist)
    ("FM宝塚 : 宝塚市 : 近畿" "simul://http://www.simulradio.info/asx/takarazuka.asx" 1 streamlist)
    ("FMビーチステーション : 白浜町 : 近畿" "simul://http://www.simulradio.info/asx/beach_station.asx" 1 streamlist)
    ("みのおエフエム : 箕面市 : 近畿" "simul://http://fm.minoh.net/minohfm.asx" 1 streamlist)
    ("YES-fm : 大阪市中央区 : 近畿" "simul://http://www.simulradio.info/asx/yes-fm.asx" 1 streamlist)
    ("京都リビングエフエム : 京都市伏見区 : 近畿" "simul://http://www.simulradio.info/asx/KyotoLivingFM.asx" 1 streamlist)
    ("さくらFM : 西宮市 : 近畿" "simul://http://www.simulradio.info/asx/sakura.asx" 1 streamlist)
    ("エフエムあまがさき : 尼崎市 : 近畿" "simul://http://www.simulradio.info/asx/aiai.asx" 1 streamlist)
    ("えふえむ草津 : 草津市 : 近畿" "simul://http://www.simulradio.info/asx/rockets785.asx" 1 streamlist)
    ("FMはしもと : 橋本市 : 近畿" "simul://http://www.simulradio.info/asx/hasimoto.asx" 1 streamlist)
    ("京都三条ラジオカフェ : 京都市中京区 : 近畿" "simul://http://www.simulradio.info/asx/radiocafe.asx" 1 streamlist)
    ("たんばしさいがいFM : 丹波市 : 近畿" "simul://http://www.simulradio.info/asx/tanbacom.asx" 1 streamlist)
    ("FMたんご : 京丹後市 : 近畿" "simul://http://www.simulradio.info/asx/tango.asx" 1 streamlist)
    ("FM MOOV KOBE : 神戸市 : 近畿" "simul://http://www.simulradio.info/asx/fmmoov.asx" 1 streamlist) )
  "Stream list of kinki area.")

(defvar emms-stream-simul-chugoku
  '(("FMちゅーピー : 広島市 : 中国" "simul://http://www.simulradio.info/asx/fm-chupea.asx" 1 streamlist)
   ("DARAZ FM : 米子市 : 中国" "simul://http://www.darazfm.com/streaming.asx" 1 streamlist))
  "Stream list of chugoku area.")

(defvar emms-stream-simul-shikoku
  '(("FM高松 : 高松市 : 四国" "simul://http://www.simulradio.info/asx/fm815.asx" 1 streamlist)
    ("FMびざん : 徳島市 : 四国" "simul://http://www.simulradio.info/asx/b-fm791.asx" 1 streamlist)
    ("FM SUN : 坂出市 : 四国" "simul://http://www.simulradio.info/asx/fmsun.asx" 1 streamlist))
  "Stream list of shikoku area.")

(defvar emms-stream-simul-kyusyu
  '(("NOAS FM : 中津市 : 九州" "simul://http://www.simulradio.info/asx/fmnakatsu.asx" 1 streamlist)
    ("サンシャイン エフエム : 宮崎市 : 九州" "simul://http://www.simulradio.info/asx/sunshinefm.asx" 1 streamlist)
    ("あまみFM : 奄美市 : 九州" "simul://http://www.npo-d.org/simul/AmamiFM.asx" 1 streamlist)
    ("FMしまばら : 島原市 : 九州" "simul://http://www.shimabara.fm/st/fm-shimabara-live.asx" 1 streamlist)
    ("FM KITAQ : 北九州市小倉北区 : 九州" "simul://http://www.shimabara.fm/st/fm-kitaq-live.asx" 1 streamlist)
    ("スターコーンFM : 築上郡築上町 : 九州" "simul://mms://hdv.nkansai.tv/starcorn" 1 streamlist)
    ("コミュニティラジオ天神 : 福岡市 : 九州" "simul://http://comiten.jp/live.asx" 1 streamlist)
    ("AIR STATION HIBIKI : 北九州市 : 九州" "simul://http://www.simulradio.info/asx/hibiki.asx" 1 streamlist)
    ("FMのべおか : 延岡市 : 九州" "simul://http://www.simulradio.info/asx/nobeoka.asx" 1 streamlist))
  "Stream list of kyusyu area.")

(defvar emms-stream-simul-okinawa
  '(("エフエム ニライ : 北谷町 : 沖縄" "simul://http://www.simulradio.info/asx/fm-nirai.asx" 1 streamlist)
    ("FMいしがき : 石垣市 : 沖縄" "simul://http://118.21.140.45/Push1" 1 streamlist)
    ("FMうるま : うるま市 : 沖縄" "simul://http://www.simulradio.info/asx/uruma.asx" 1 streamlist)
    ("FM21 : 浦添市 : 沖縄" "simul://http://www.simulradio.info/asx/fm21.asx" 1 streamlist)
    ("FMレキオ : 那覇市 : 沖縄" "simul://http://www.simulradio.info/asx/lequio.asx" 1 streamlist)
    ("FMとよみ : 豊見城市 : 沖縄" "simul://http://www.simulradio.info/asx/toyomi.asx" 1 streamlist)
    ("オキラジ : 沖縄市 : 沖縄" "simul://http://www.simulradio.info/asx/okiradi.asx" 1 streamlist)
    ("FMなんじょう : 南城市 : 沖縄" "simul://http://www.simulradio.info/asx/nanjo.asx" 1 streamlist)
    ("FMもとぶ : 本部町 : 沖縄" "simul://http://www.simulradio.info/asx/motob.asx" 1 streamlist)
    ("FMくめじま : 久米島 : 沖縄" "simul://http://www.simulradio.info/asx/fmkumejima.asx" 1 streamlist))
  "Stream list of okinawa area.")

(defvar emms-stream-simul-streams-name
  '(emms-stream-simul-hokkaido
    emms-stream-simul-touhoku
    emms-stream-simul-shinetsu
    emms-stream-simul-kantou
    emms-stream-simul-toukai
    emms-stream-simul-hokuriku
    emms-stream-simul-kinki
    emms-stream-simul-chugoku
    emms-stream-simul-shikoku
    emms-stream-simul-kyusyu
    emms-stream-simul-okinawa)
  "Symbol list of SimulRadio streams name.")

(defun emms-stream-simul-add-bookmark-1 (&rest simul-streams-name)
  "Helper functions for `emms-stream-simul-add-bookmark', etc."
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (let* ((line  (emms-line-number-at-pos (point)))
         (index (+ (/ line 2) 1)))
    (dolist (streams-name simul-streams-name)
      (dolist (stream (symbol-value streams-name))
        (setq emms-stream-list (emms-stream-insert-at index stream
                                                      emms-stream-list))
        (cl-incf index)))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun emms-stream-simul-get-stream-list ()
  "Return new stream-list."
  (cl-loop
   with ls = nil
   for streams-name in emms-stream-simul-streams-name do
   (dolist (stream (symbol-value streams-name))
     (push stream ls))
   finally return (nreverse ls)))

;;;###autoload
(defun emms-stream-simul-add-bookmark (&optional location)
  "Create simul bookmark, and insert it at point position.
LOCATION is a number of 0-11.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive)
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (unless (integerp location)
    (let ((msg (concat "[0] All  [1] 北海道(Hokkaido)  [2] 東北(Touhoku)  [3] 信州(Shinetsu)\n"
                       "         [4] 関東(Kantou)      [5] 東海(Toukai)   [6] 北陸(Hokuriku)\n"
                       "         [7] 近畿(kinki)       [8] 中国(Chugoku)  [9] 四国(Shikoku)\n"
                       "        [10] 九州(kyusyu)     [11] 沖縄(Okinawa)\n\n"
                       "Input a number of 0-11: ")))
      (while (not (and (integerp (setq location (read-number msg)))
                       (<= 0 location) (<= location 11))))))
  (if (zerop location)
      (apply #'emms-stream-simul-add-bookmark-1 emms-stream-simul-streams-name)
    (emms-stream-simul-add-bookmark-1 (nth (1- location) emms-stream-simul-streams-name))))

;; For media player

(cl-defun emms-stream-simul--xml-collect-node
    (name xml-ls &key (test #'identity) (getter #'identity))
  "Collect nodes of NAME from XML-LS.
TEST and GETTER takes a node of NAME as an argument.
TEST is a predicate function.
Object returned by GETTER is collected."
  (cl-labels ((collect-name-node (xml-ls ls)
                (cond
                 ((atom xml-ls) ls)
                 ((consp (car xml-ls))
                  (collect-name-node
                   (car xml-ls)
                   (collect-name-node (cdr xml-ls) ls)))
                 ((and (eq (car xml-ls) name)
                       (funcall test xml-ls))
                  (cons (funcall getter xml-ls) ls))
                 ((or (null (car xml-ls))
                      (not (symbolp (car xml-ls))))
                  (collect-name-node (cdr xml-ls) ls))
                 ((symbolp (car xml-ls))
                  (collect-name-node (xml-node-children xml-ls) ls ))
                 (t ls))))
    (collect-name-node xml-ls nil)))

(defun emms-stream-simul--asx-to-href (asx)
  "Return href from ASX."
  (let* ((buf (url-retrieve-synchronously asx))
         (html (with-current-buffer buf
                 (goto-char (point-min))
                 (while (and (not (eobp)) (not (eolp))) (forward-line 1))
                 (unless (eobp) (forward-line 1))
                 (prog1 (libxml-parse-html-region (point) (point-max))
                   (kill-buffer buf)))))
    (car (last (emms-stream-simul--xml-collect-node
                'ref html
                :test
                (lambda (node) (xml-get-attribute-or-nil node 'href))
                :getter
                (lambda (node) (xml-get-attribute node 'href)))))))

(defun emms-stream-simul-stream-url-to-url (stream-url)
  "Replace simul:\\ of STREAM-URL with empty string."
  (replace-regexp-in-string "\\`s\\(imul\\|aimaru\\)://" "" stream-url))

(defun emms-stream-simul-stream-url-to-asx-ref (stream-url)
  "Return ref of asx from STREAM-URL."
  (emms-stream-simul--asx-to-href
   (emms-stream-simul-stream-url-to-url stream-url)))

(provide 'emms-streams-simul)
;;; emms-streams-simul.el ends here
