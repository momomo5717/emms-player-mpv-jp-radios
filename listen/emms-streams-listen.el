;;; emms-streams-listen.el --- emms stream list for ListenRadio -*- lexical-binding: t -*-

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

;; This provides emms stream list for ListenRadio.

;;; Code:
(require 'emms-streams)
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'json)

(defun emms-stream-listen--fetch-json-obj (url)
  "Return a json object from URL."
  (let* ((buf (url-retrieve-synchronously url))
         ;; Decoding string
         (html
          (with-current-buffer buf
            (goto-char (point-min))
            (while (and (not (eobp)) (not (eolp))) (forward-line 1))
            (unless (eobp) (forward-line 1))
            (libxml-parse-html-region (point) (point-max))))
         (body (car (xml-get-children html 'body)))
         (p (car (xml-get-children body 'p)))
         (json-str (car (xml-node-children p))))
    (prog1 (with-temp-buffer
             (insert json-str)
             (goto-char (point-min))
             (json-read))
      (kill-buffer buf))))

(defvar emms-stream-listen--category-alist
  '((10002 . "音楽ジャンル")
    (10007 . "音楽ブランド")
    (10004 . "シチュエーション")
    (10003 . "気持ち")
    (10008 . "バラエティ")
    (10005 . "全国のラジオ局")
    (99999 . "すべてのチャンネル")))

(defvar emms-stream-listen--area-alist
  '((1 . "北海道")
    (2 . "東北")
    (3 . "関東")
    (4 . "東海")
    (5 . "北信越")
    (6 . "近畿")
    (7 . "中国・四国")
    (8 . "九州・沖縄")))

(defun emms-stream-listen--fetch-category-streams (id)
  "Return stream list or alist for ID."
  (cl-loop
   with area-alist = emms-stream-listen--area-alist
   with streams = (if (eq id 10005)
                      (cl-loop for i from 1 to 8 collect
                               (list (cdr (assq i area-alist))))
                    nil)
   with url =
   (format "http://listenradio.jp/service/categorychannel.aspx?categoryid=%s"
           id)
   with category-name = (cdr (assq id emms-stream-listen--category-alist))
   for category across (cdr (assq 'Channel
                                  (emms-stream-listen--fetch-json-obj url)))
   for name   = (cdr (assq 'ChannelName category))
   for m3u8   = (cdr (assq 'ChannelHls  category))
   for chId   = (cdr (assq 'ChannelId category))
   for area   = (cdr (assq (cdr (assq 'AreaId category))
                           emms-stream-listen--area-alist))
   for stream = (list (concat name (if area (concat " : " area) "")
                              " : " category-name)
                      (format "listen://%s" chId) 1 'streamlist)
   when (string-match-p "[.]m3u8$" m3u8) do
   (if (eq id 10005) (nconc (assoc area streams) (list stream))
     (push stream streams))
   finally return
   (if (not (eq id 10005)) (nreverse streams)
     (cl-loop for (area . area-streams) in streams
              collect (cons (car (rassoc area area-alist)) area-streams)))))

(defun emms-stream-listen--featch-stream-alist ()
  "Return `emms-stream-listen--stream-alist' to update."
  (cl-loop
   for (id . _) in emms-stream-listen--category-alist
   unless (eq id 99999)
   collect (cons id (emms-stream-listen--fetch-category-streams id))))

(defvar emms-stream-listen--stream-alist
  '((10002 ("カフェミュージック : 音楽ジャンル" "listen://20088" 1 streamlist)
           ("ラブソング : 音楽ジャンル" "listen://20006" 1 streamlist)
           ("オルゴール : 音楽ジャンル" "listen://20012" 1 streamlist)
           ("ジャズ : 音楽ジャンル" "listen://20010" 1 streamlist)
           ("デジタルクラシック : 音楽ジャンル" "listen://20009" 1 streamlist)
           ("自然音 : 音楽ジャンル" "listen://20097" 1 streamlist)
           ("ヒーリング : 音楽ジャンル" "listen://20011" 1 streamlist)
           ("イージーリスニング : 音楽ジャンル" "listen://20076" 1 streamlist)
           ("アニメ/ゲーム : 音楽ジャンル" "listen://20007" 1 streamlist)
           ("ポップス : 音楽ジャンル" "listen://20001" 1 streamlist)
           ("ロック : 音楽ジャンル" "listen://20003" 1 streamlist)
           ("ヒップホップ/R&B : 音楽ジャンル" "listen://20002" 1 streamlist)
           ("レゲエ : 音楽ジャンル" "listen://20004" 1 streamlist)
           ("クラブ/ダンス : 音楽ジャンル" "listen://20005" 1 streamlist)
           ("歌謡曲 : 音楽ジャンル" "listen://20008" 1 streamlist)
           ("蛍の光 : 音楽ジャンル" "listen://20079" 1 streamlist))
    (10007 ("アイドルーチュ！ : 音楽ブランド" "listen://20096" 1 streamlist)
           ("ロッカーズアイランド : 音楽ブランド" "listen://20080" 1 streamlist)
           ("IRMA records : 音楽ブランド" "listen://20084" 1 streamlist)
           ("Riddim Nation : 音楽ブランド" "listen://20071" 1 streamlist)
           ("Fan Music富ヶ谷 : 音楽ブランド" "listen://20074" 1 streamlist)
           ("PoNY TAiL : 音楽ブランド" "listen://20067" 1 streamlist)
           ("MUMIX Radio : 音楽ブランド" "listen://20069" 1 streamlist)
           ("VAA : 音楽ブランド" "listen://20066" 1 streamlist)
           ("WASABEAT : 音楽ブランド" "listen://20065" 1 streamlist)
           ("Wmiba : 音楽ブランド" "listen://20064" 1 streamlist)
           ("アスノカケハシ : 音楽ブランド" "listen://20070" 1 streamlist)
           ("みよー : 音楽ブランド" "listen://20068" 1 streamlist))
    (10004 ("目が覚めたとき : シチュエーション" "listen://20013" 1 streamlist)
           ("ドライブするとき : シチュエーション" "listen://20015" 1 streamlist)
           ("食卓やキッチンで : シチュエーション" "listen://20014" 1 streamlist)
           ("ランニングしながら : シチュエーション" "listen://20017" 1 streamlist)
           ("勉強中・仕事中に : シチュエーション" "listen://20016" 1 streamlist)
           ("掃除や洗濯しながら : シチュエーション" "listen://20018" 1 streamlist)
           ("寝室やリビングで : シチュエーション" "listen://20021" 1 streamlist)
           ("お酒を飲んでいるとき : シチュエーション" "listen://20020" 1 streamlist)
           ("のんびりしてるとき : シチュエーション" "listen://20019" 1 streamlist))
    (10003 ("恋してるときに : 気持ち" "listen://20022" 1 streamlist)
           ("テンションあげたいときに : 気持ち" "listen://20023" 1 streamlist)
           ("リラックスしたいときに : 気持ち" "listen://20024" 1 streamlist)
           ("落ち込んでいるときに : 気持ち" "listen://20025" 1 streamlist)
           ("集中したいときに : 気持ち" "listen://20026" 1 streamlist)
           ("イライラしてるときに : 気持ち" "listen://20027" 1 streamlist)
           ("疲れているときに : 気持ち" "listen://20028" 1 streamlist)
           ("暇を持て余しているときに : 気持ち" "listen://20029" 1 streamlist)
           ("現実逃避したいときに : 気持ち" "listen://20030" 1 streamlist))
    (10008 ("新刊ラジオ : バラエティ" "listen://20098" 1 streamlist)
           ("あすからココナッツ書店（営業中） : バラエティ" "listen://20099" 1 streamlist)
           ("耳で聴くビジネス書 オーディオブック : バラエティ" "listen://20063" 1 streamlist)
           ("耳で聴く童話・むかしばなし オーディオブック : バラエティ" "listen://20073" 1 streamlist)
           ("耳で聴く名作 オーディオブック : バラエティ" "listen://20078" 1 streamlist)
           ("もしも声優がいっぱい住んでいるマンションがあったら : バラエティ" "listen://20095" 1 streamlist)
           ("Owarai.radio : バラエティ" "listen://20072" 1 streamlist))
    (10005 (1 ("FMりべーる : 北海道 : 全国のラジオ局" "listen://30074" 1 streamlist)
              ("FMくしろ : 北海道 : 全国のラジオ局" "listen://30029" 1 streamlist)
              ("FM JAGA : 北海道 : 全国のラジオ局" "listen://30016" 1 streamlist)
              ("FM WING : 北海道 : 全国のラジオ局" "listen://30038" 1 streamlist)
              ("Radio D FM dramacity : 北海道 : 全国のラジオ局" "listen://30044" 1 streamlist)
              ("三角山放送局 : 北海道 : 全国のラジオ局" "listen://30005" 1 streamlist)
              ("ラジオカロスサッポロ : 北海道 : 全国のラジオ局" "listen://30034" 1 streamlist)
              ("FMアップル : 北海道 : 全国のラジオ局" "listen://30090" 1 streamlist)
              ("ラジオニセコ : 北海道 : 全国のラジオ局" "listen://30060" 1 streamlist)
              ("FMいるか : 北海道 : 全国のラジオ局" "listen://30047" 1 streamlist))
           (2 ("BeFM : 東北 : 全国のラジオ局" "listen://30079" 1 streamlist)
              ("鹿角きりたんぽFM : 東北 : 全国のラジオ局" "listen://30089" 1 streamlist)
              ("横手かまくらエフエム : 東北 : 全国のラジオ局" "listen://30076" 1 streamlist)
              ("FMゆーとぴあ : 東北 : 全国のラジオ局" "listen://30030" 1 streamlist)
              ("ラヂオもりおか : 東北 : 全国のラジオ局" "listen://30017" 1 streamlist)
              ("みやこハーバーラジオ : 東北 : 全国のラジオ局" "listen://30097" 1 streamlist)
              ("おおつちさいがいエフエム : 東北 : 全国のラジオ局" "listen://30098" 1 streamlist)
              ("かまいしさいがいエフエム : 東北 : 全国のラジオ局" "listen://30087" 1 streamlist)
              ("FMねまらいん : 東北 : 全国のラジオ局" "listen://30045" 1 streamlist)
              ("陸前高田災害FM : 東北 : 全国のラジオ局" "listen://30095" 1 streamlist)
              ("けせんぬまさいがいエフエム : 東北 : 全国のラジオ局" "listen://30094" 1 streamlist)
              ("女川さいがいFM : 東北 : 全国のラジオ局" "listen://30096" 1 streamlist)
              ("ラジオ石巻 : 東北 : 全国のラジオ局" "listen://30037" 1 streamlist)
              ("BAY WAVE : 東北 : 全国のラジオ局" "listen://30056" 1 streamlist)
              ("fmいずみ : 東北 : 全国のラジオ局" "listen://30018" 1 streamlist)
              ("RADIO3 : 東北 : 全国のラジオ局" "listen://30007" 1 streamlist)
              ("なとらじ801 : 東北 : 全国のラジオ局" "listen://30092" 1 streamlist)
              ("FMあおぞら : 東北 : 全国のラジオ局" "listen://30093" 1 streamlist)
              ("りんごラジオ : 東北 : 全国のラジオ局" "listen://30058" 1 streamlist)
              ("南相馬ひばりFM : 東北 : 全国のラジオ局" "listen://30091" 1 streamlist)
              ("ＦＭ－ＭｏｔＣｏｍ : 東北 : 全国のラジオ局" "listen://30019" 1 streamlist)
              ("FM 愛’ｓ : 東北 : 全国のラジオ局" "listen://30031" 1 streamlist)
              ("おだがいさまFM : 東北 : 全国のラジオ局" "listen://30099" 1 streamlist)
              ("KOCOラジ : 東北 : 全国のラジオ局" "listen://30020" 1 streamlist)
              ("FMいわき : 東北 : 全国のラジオ局" "listen://30009" 1 streamlist))
           (3 ("たかはぎFM : 関東 : 全国のラジオ局" "listen://30075" 1 streamlist)
              ("FMぱるるん : 関東 : 全国のラジオ局" "listen://30022" 1 streamlist)
              ("ラヂオつくば : 関東 : 全国のラジオ局" "listen://30021" 1 streamlist)
              ("まえばしCITYエフエム : 関東 : 全国のラジオ局" "listen://30043" 1 streamlist)
              ("フラワーラジオ : 関東 : 全国のラジオ局" "listen://30002" 1 streamlist)
              ("調布FM : 関東 : 全国のラジオ局" "listen://30039" 1 streamlist)
              ("REDS WAVE : 関東 : 全国のラジオ局" "listen://30008" 1 streamlist)
              ("すまいるエフエム : 関東 : 全国のラジオ局" "listen://30026" 1 streamlist)
              ("レインボータウンFM : 関東 : 全国のラジオ局" "listen://30036" 1 streamlist)
              ("FMたちかわ : 関東 : 全国のラジオ局" "listen://30033" 1 streamlist)
              ("かわさきＦＭ : 関東 : 全国のラジオ局" "listen://30046" 1 streamlist)
              ("FMサルース : 関東 : 全国のラジオ局" "listen://30061" 1 streamlist)
              ("エフエム戸塚 : 関東 : 全国のラジオ局" "listen://30064" 1 streamlist)
              ("エフエムさがみ : 関東 : 全国のラジオ局" "listen://30080" 1 streamlist)
              ("FMやまと : 関東 : 全国のラジオ局" "listen://30014" 1 streamlist)
              ("ＦＭカオン : 関東 : 全国のラジオ局" "listen://30057" 1 streamlist)
              ("レディオ湘南 : 関東 : 全国のラジオ局" "listen://30063" 1 streamlist)
              ("湘南ビーチFM : 関東 : 全国のラジオ局" "listen://30028" 1 streamlist))
           (4 ("Ciao! : 東海 : 全国のラジオ局" "listen://30062" 1 streamlist)
              ("MID FM : 東海 : 全国のラジオ局" "listen://30004" 1 streamlist)
              ("RADIO LOVEAT : 東海 : 全国のラジオ局" "listen://30081" 1 streamlist)
              ("Pitch FM : 東海 : 全国のラジオ局" "listen://30065" 1 streamlist)
              ("FMおかざき : 東海 : 全国のラジオ局" "listen://30040" 1 streamlist)
              ("ポートウェーブ（FMよっかいち） : 東海 : 全国のラジオ局" "listen://30015" 1 streamlist))
           (5 ("FM KENTO : 北信越 : 全国のラジオ局" "listen://30042" 1 streamlist)
              ("ＦＭ軽井沢 : 北信越 : 全国のラジオ局" "listen://30032" 1 streamlist)
              ("あづみ野エフエム : 北信越 : 全国のラジオ局" "listen://30086" 1 streamlist)
              ("ラジオ・ミュー : 北信越 : 全国のラジオ局" "listen://30006" 1 streamlist)
              ("FM N1 : 北信越 : 全国のラジオ局" "listen://30001" 1 streamlist)
              ("敦賀FM : 北信越 : 全国のラジオ局" "listen://30012" 1 streamlist))
           (6 ("FM丹波 : 近畿 : 全国のラジオ局" "listen://30027" 1 streamlist)
              ("京都三条ラジオカフェ : 近畿 : 全国のラジオ局" "listen://30082" 1 streamlist)
              ("エフエムひらかた : 近畿 : 全国のラジオ局" "listen://30049" 1 streamlist)
              ("FM千里 : 近畿 : 全国のラジオ局" "listen://30048" 1 streamlist)
              ("ＦＭ ＨＡＮＡＫＯ : 近畿 : 全国のラジオ局" "listen://30068" 1 streamlist)
              ("YES-fm : 近畿 : 全国のラジオ局" "listen://30073" 1 streamlist)
              ("FM　TANABE : 近畿 : 全国のラジオ局" "listen://30051" 1 streamlist)
              ("ビーチステーション : 近畿 : 全国のラジオ局" "listen://30084" 1 streamlist)
              ("ＦＭ　aiai  : 近畿 : 全国のラジオ局" "listen://30067" 1 streamlist)
              ("さくらFM : 近畿 : 全国のラジオ局" "listen://30100" 1 streamlist)
              ("FMわぃわぃ : 近畿 : 全国のラジオ局" "listen://30025" 1 streamlist)
              ("BAN-BANラジオ : 近畿 : 全国のラジオ局" "listen://30078" 1 streamlist)
              ("FM GENKI : 近畿 : 全国のラジオ局" "listen://30041" 1 streamlist)
              ("FM ジャングル : 近畿 : 全国のラジオ局" "listen://30013" 1 streamlist))
           (7 ("DARAZ　FM : 中国・四国 : 全国のラジオ局" "listen://30053" 1 streamlist)
              ("FM815（高松） : 中国・四国 : 全国のラジオ局" "listen://30024" 1 streamlist)
              ("FM SUN : 中国・四国 : 全国のラジオ局" "listen://30070" 1 streamlist)
              ("FMびざん : 中国・四国 : 全国のラジオ局" "listen://30010" 1 streamlist))
           (8 ("AIR STATION HIBIKI : 九州・沖縄 : 全国のラジオ局" "listen://30052" 1 streamlist)
              ("StarCorn FM : 九州・沖縄 : 全国のラジオ局" "listen://30085" 1 streamlist)
              ("宮崎サンシャインエフエム : 九州・沖縄 : 全国のラジオ局" "listen://30023" 1 streamlist)
              ("FMのべおか : 九州・沖縄 : 全国のラジオ局" "listen://30088" 1 streamlist)
              ("あまみエフエム : 九州・沖縄 : 全国のラジオ局" "listen://30054" 1 streamlist)
              ("FMもとぶ : 九州・沖縄 : 全国のラジオ局" "listen://30072" 1 streamlist)
              ("FMうるま : 九州・沖縄 : 全国のラジオ局" "listen://30011" 1 streamlist)
              ("オキラジ : 九州・沖縄 : 全国のラジオ局" "listen://30066" 1 streamlist)
              ("FMニライ : 九州・沖縄 : 全国のラジオ局" "listen://30003" 1 streamlist)
              ("FM 21 : 九州・沖縄 : 全国のラジオ局" "listen://30059" 1 streamlist)
              ("FMレキオ : 九州・沖縄 : 全国のラジオ局" "listen://30071" 1 streamlist)
              ("FMとよみ : 九州・沖縄 : 全国のラジオ局" "listen://30083" 1 streamlist)
              ("FMなんじょう : 九州・沖縄 : 全国のラジオ局" "listen://30055" 1 streamlist)
              ("FMくめじま : 九州・沖縄 : 全国のラジオ局" "listen://30077" 1 streamlist)
              ("FMいしがきサンサンラジオ : 九州・沖縄 : 全国のラジオ局" "listen://30069" 1 streamlist)))))

(defun emms-stream-listen--get-stream-list (id &optional area)
  "Return stream list of ID.
If ID is 10005, AREA is required."
  (let ((stream-list (cdr (assq id emms-stream-listen--stream-alist))))
    (if (eq id 10005) (cdr (assq area stream-list)) stream-list)))

(defun emms-stream-listen--add-bookmark-1 (ids)
  "Helper function for `emms-stream-listen-add-bookmark'.
IDS = (id ...)
id  = number or (number ...)"
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (let* ((line  (emms-line-number-at-pos (point)))
         (index (+ (/ line 2) 1)))
    (cl-loop
     with areas = nil
     for id in ids
     ;; For id = '(10005 area-id ...) or 10005
     if (or (listp id) (eq id 10005)) do
     (setq areas (if (listp id) (cdr id)
                   (mapcar #'car emms-stream-listen--area-alist)))
     (setq id (if (listp id) (car id) id))
     (dolist (area areas)
       (dolist (stream (emms-stream-listen--get-stream-list id area))
         (setq emms-stream-list
               (emms-stream-insert-at index stream emms-stream-list))
         (cl-incf index)))
     else
     for streams = (emms-stream-listen--get-stream-list id) do
     (dolist (stream streams)
       (setq emms-stream-list
             (emms-stream-insert-at index stream emms-stream-list))
       (cl-incf index)))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun emms-stream-listen-add-bookmark (&optional category area)
  "Create listen bookmark, and insert it at point position.
CATEGORY is a number of 0-6.
AREA is a number of 0-8.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive)
  (unless (integerp category)
    (let ((msg (concat "[0] All  [1] 音楽ジャンル  [2] 音楽ブランド  [3] シチュエーション\n"
                       "         [4] 気持ち        [5] バラエティ    [6] 全国のラジオ局\n\n"
                       "Input a number of 0-6: ")))
      (while (not (and (integerp (setq category (read-number msg))) (<= 0 category 6))))))
  (when (and (eq category 6) (null area))
    (let ((msg (concat "[0] All  [1] 北海道  [2] 東北  [3] 関東        [4] 東海\n"
                       "         [5] 北信越  [6] 近畿  [7] 中国・四国  [8] 九州・沖縄\n\n"
                       "Input a number of 0-8: ")))
      (while (not (and (integerp (setq area (read-number msg))) (<= 0 area 8))))))
  (let ((id-ls (mapcar #'car emms-stream-listen--category-alist)))
   (cond
    ((zerop category) (emms-stream-listen--add-bookmark-1 id-ls))
    (area (if (zerop area)
              (emms-stream-listen--add-bookmark-1 (list (nth (1- category) id-ls)))
            (emms-stream-listen--add-bookmark-1 `((,(nth (1- category) id-ls) ,area)))))
    (t (emms-stream-listen--add-bookmark-1 (list (nth (1- category) id-ls)))))))

(provide 'emms-streams-listen)
;;; emms-streams-listen.el ends here
