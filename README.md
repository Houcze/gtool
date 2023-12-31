# gtool

Please note that the code in this repository is only a branch of <https://www.gfd-dennou.org/library/gtool/gtool5.htm.ja>.

I had to make some modifications to the Makefile in order to make this package still buildable in 2023.

gtool5 -- Gtool5 Fortran 90/95 Library

***Note that Japanese and English are described in parallel***

gtool5 に関しては, doc/index.htm もしくは
<http://www.gfd-dennou.org/library/gtool/gtool5/gtool5_current/doc>
をご覧ください.

See doc/index.htm.en or
<http://www.gfd-dennou.org/library/gtool/gtool5/gtool5_current/doc/index.htm.en>
for gtool5

= Installation

INSTALL.htm もしくは INSTALL をご覧ください.

See INSTALL.htm.en (or INSTALL).

= COPYRIGHT

COPYRIGHT をご覧ください.

See COPYRIGHT.

= Contents

本パッケージは以下のファイル, ディレクトリから構成されています.

- README:    本ファイル
- COPYRIGHT: 著作権が記載されたファイル
- VERSION:   バージョン情報
- INSTALL,
    INSTALL.htm:        gtool5 インストールガイド
- INSTALL_netcdf,
    INSTALL_netcdf.htm: netCDF インストールガイド
- HISTORY,
    HISTORY.htm:  開発履歴
- CREDITS,
    CREDITS.htm:  使用上の注意, 開発者一覧

- src/:         ソースコードを収めたディレクトリ.
                      詳細は src/SRC_LIST を参照のこと
- doc/:         ドキュメントを収めたディレクトリ.
                      詳細は doc/index.htm を参照のこと
- gt5frt/:      gt5frt, gt5config コマンドの生成スクリプトを
                      収めたディレクトリ

- Makefile:     Makefileの本体
- Config.mk.in: 各ディレクトリの Makefile において読み込まれる
                      インクルードファイル Config.mk の雛型.
                      Config.mk は configure スクリプトによって作成される.
                      Config.mk には主に環境変数が記述される
- rules.make:   各ディレクトリの Makefile において読み込まれる
                      インクルードファイル.
                      共通利用されるルールが記述される
- configure,
    config.guess,
    config.sub,
    install-sh:   Config.mk を作成するためのスクリプト
- configure.in: configure スクリプトの雛型 (開発者用). このファイルを元に,
                      autoconf コマンドで configure が作成される

- htmltools/:   HTML 関係のファイル置場
- script/:      スクリプト置場
- test/:        動作確認用のソースコード, シェルスクリプト置場

- ChangeLog:    変更履歴の詳細 (開発者用)

This package is consist of following files and directories.

- README:    This file
- COPYRIGHT: File in which copyright is described
- VERSION:   Version information
- INSTALL,
    INSTALL.htm:        Gtool5 installation guide
- INSTALL_netcdf,
    INSTALL_netcdf.htm: NetCDF installation guide
- HISTORY,
    HISTORY.htm.en: Development history
- CREDITS,
    CREDITS.htm.en: Precautionary statement, list of developers

- src/:       Directory in which source codes are located.
                    See src/SRC_LIST for details
- doc/:       Directory in which documents are located.
                    See doc/index.htm.en for details
- gt5frt/:    Directory in which scripts for generation of
                    "gt5frt", "gt5config" are located.

- Makefile:     Main Makefile
- Config.mk.in: Template file for Config.mk that is included
                      by Makefile in each directory.
                      Config.mk is generated by configure script.
                      Environment variables are described in
                      Config.mk mainly.
- rules.make:   This file is included by Makefile in each directory.
                      Common rules are described in this file mainly.
- configure,
    config.guess,
    config.sub,
    install-sh:   Scripts for generation of Config.mk
- configure.in: Template file for configure (for developers)
                      By autoconf command and this file,
                      configure is generated.

- htmltools/:   Directory in which files for HTML are located
- script/:      Directory in which scripts are located
- test/:        Directory in which source codes and scripts for tests are located

- ChangeLog:    Details of development history (for developers)

= Contact

このパッケージに対するコメント, 提案, バグレポートは
<dcstaff_at_gfd-dennou.org> ("*at*" は "@" に置き換えてください)
までお寄せください. その際には, VERSION ファイルに記載される
バージョン情報も付記ください.

Please send comments, suggestions, and bug reports to
<dcstaff_at_gfd-dennou.org> (Please replace "*at*" with "@").
Please identify the version of the package (file VERSION).
