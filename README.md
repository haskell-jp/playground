playground
====

様々な具体例を集積する遊び場です。以下のような状況に役立つものを目指していきましょう。

* 言語拡張、ライブラリ、技法などを使ってみたいけれどネタがない
* 新しいライブラリを作ったので、テストするためのデータが欲しい
* とにかく色々な例を参考にしたい
* ひどい罠にかかったので、今後引っかからないように記録したい(→`src/Pitfall.hs`)

コンテンツ
----

* Calc.Base: 数式を代数的データ型で表す
* Calc.TTG: Trees That Growを利用して数式のデータ型を定義する
* UserData.Types: 文字列、数値、他のデータ型などを絡めたレコードの例
* Pitfall: 落とし穴
* Generics.FieldName: レコードのフィールド名をまとめてHKDとして取得する
* Extension.DerivingVia: `DerivingVia`拡張の例(aeson)
* Streaming.Experimental: `drinkery`の前身であった試製ストリームライブラリ
* `app/cbor-tool.hs`: CBORを色付きでプリティプリントする例(cborg)
* `app/harvester.hs`: parsers/trifectaとfoldlパッケージを使った、コマンドラインの簡易統計ツール

コントリビューション
----

Pull requestもしくは、権限がある方は直接pushしてもかまいません。
「こんな例が欲しい」といったリクエストも歓迎です。
