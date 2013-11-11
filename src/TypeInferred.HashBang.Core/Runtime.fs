[<ReflectedDefinition>]
module TypeInferred.HashBang.Runtime

type PrimitiveType =
    | BoolType
    | IntType
    | StringType
    | UIntType
    //| HexType
    //| OctalType
    | FloatType
    | DecimalType
    | CharType

type UrlPart =
    | FixedPart of string
    | VariablePart of string * PrimitiveType

type ContentType = 
    | ContentType of string
    member x.Mime = 
        let (ContentType mime) = x
        mime
module ContentTypes =
    let lookupByExtension =
        Map [|
            "N/A", ContentType "application/andrew-inset"
            ".aw", ContentType "application/applixware"
            ".atom, .xml", ContentType "application/atom+xml"
            ".atomcat", ContentType "application/atomcat+xml"
            ".atomsvc", ContentType "application/atomsvc+xml"
            ".ccxml", ContentType "application/ccxml+xml,"
            ".cdmia", ContentType "application/cdmi-capability"
            ".cdmic", ContentType "application/cdmi-container"
            ".cdmid", ContentType "application/cdmi-domain"
            ".cdmio", ContentType "application/cdmi-object"
            ".cdmiq", ContentType "application/cdmi-queue"
            ".cu", ContentType "application/cu-seeme"
            ".davmount", ContentType "application/davmount+xml"
            ".dssc", ContentType "application/dssc+der"
            ".xdssc", ContentType "application/dssc+xml"
            ".es", ContentType "application/ecmascript"
            ".emma", ContentType "application/emma+xml"
            ".epub", ContentType "application/epub+zip"
            ".exi", ContentType "application/exi"
            ".pfr", ContentType "application/font-tdpfr"
            ".stk", ContentType "application/hyperstudio"
            ".ipfix", ContentType "application/ipfix"
            ".jar", ContentType "application/java-archive"
            ".ser", ContentType "application/java-serialized-object"
            ".class", ContentType "application/java-vm"
            ".js", ContentType "application/javascript"
            ".json", ContentType "application/json"
            ".mads", ContentType "application/mads+xml"
            ".mrc", ContentType "application/marc"
            ".mrcx", ContentType "application/marcxml+xml"
            ".ma", ContentType "application/mathematica"
            ".mathml", ContentType "application/mathml+xml"
            ".mbox", ContentType "application/mbox"
            ".mscml", ContentType "application/mediaservercontrol+xml"
            ".meta4", ContentType "application/metalink4+xml"
            ".mets", ContentType "application/mets+xml"
            ".mods", ContentType "application/mods+xml"
            ".m21", ContentType "application/mp21"
            ".mp4", ContentType "application/mp4"
            ".doc", ContentType "application/msword"
            ".mxf", ContentType "application/mxf"
            ".bin", ContentType "application/octet-stream"
            ".oda", ContentType "application/oda"
            ".opf", ContentType "application/oebps-package+xml"
            ".ogx", ContentType "application/ogg"
            ".onetoc", ContentType "application/onenote"
            ".xer", ContentType "application/patch-ops-error+xml"
            ".pdf", ContentType "application/pdf"
            "", ContentType "application/pgp-encrypted"
            ".pgp", ContentType "application/pgp-signature"
            ".prf", ContentType "application/pics-rules"
            ".p10", ContentType "application/pkcs10"
            ".p7m", ContentType "application/pkcs7-mime"
            ".p7s", ContentType "application/pkcs7-signature"
            ".p8", ContentType "application/pkcs8"
            ".ac", ContentType "application/pkix-attr-cert"
            ".cer", ContentType "application/pkix-cert"
            ".crl", ContentType "application/pkix-crl"
            ".pkipath", ContentType "application/pkix-pkipath"
            ".pki", ContentType "application/pkixcmp"
            ".pls", ContentType "application/pls+xml"
            ".ai", ContentType "application/postscript"
            ".cww", ContentType "application/prs.cww"
            ".pskcxml", ContentType "application/pskc+xml"
            ".rdf", ContentType "application/rdf+xml"
            ".rif", ContentType "application/reginfo+xml"
            ".rnc", ContentType "application/relax-ng-compact-syntax"
            ".rl", ContentType "application/resource-lists+xml"
            ".rld", ContentType "application/resource-lists-diff+xml"
            ".rs", ContentType "application/rls-services+xml"
            ".rsd", ContentType "application/rsd+xml"
            ".rss, .xml", ContentType "application/rss+xml"
            ".rtf", ContentType "application/rtf"
            ".sbml", ContentType "application/sbml+xml"
            ".scq", ContentType "application/scvp-cv-request"
            ".scs", ContentType "application/scvp-cv-response"
            ".spq", ContentType "application/scvp-vp-request"
            ".spp", ContentType "application/scvp-vp-response"
            ".sdp", ContentType "application/sdp"
            ".setpay", ContentType "application/set-payment-initiation"
            ".setreg", ContentType "application/set-registration-initiation"
            ".shf", ContentType "application/shf+xml"
            ".smi", ContentType "application/smil+xml"
            ".rq", ContentType "application/sparql-query"
            ".srx", ContentType "application/sparql-results+xml"
            ".gram", ContentType "application/srgs"
            ".grxml", ContentType "application/srgs+xml"
            ".sru", ContentType "application/sru+xml"
            ".ssml", ContentType "application/ssml+xml"
            ".tei", ContentType "application/tei+xml"
            ".tfi", ContentType "application/thraud+xml"
            ".tsd", ContentType "application/timestamped-data"
            ".plb", ContentType "application/vnd.3gpp.pic-bw-large"
            ".psb", ContentType "application/vnd.3gpp.pic-bw-small"
            ".pvb", ContentType "application/vnd.3gpp.pic-bw-var"
            ".tcap", ContentType "application/vnd.3gpp2.tcap"
            ".pwn", ContentType "application/vnd.3m.post-it-notes"
            ".aso", ContentType "application/vnd.accpac.simply.aso"
            ".imp", ContentType "application/vnd.accpac.simply.imp"
            ".acu", ContentType "application/vnd.acucobol"
            ".atc", ContentType "application/vnd.acucorp"
            ".air", ContentType "application/vnd.adobe.air-application-installer-package+zip"
            ".fxp", ContentType "application/vnd.adobe.fxp"
            ".xdp", ContentType "application/vnd.adobe.xdp+xml"
            ".xfdf", ContentType "application/vnd.adobe.xfdf"
            ".ahead", ContentType "application/vnd.ahead.space"
            ".azf", ContentType "application/vnd.airzip.filesecure.azf"
            ".azs", ContentType "application/vnd.airzip.filesecure.azs"
            ".azw", ContentType "application/vnd.amazon.ebook"
            ".acc", ContentType "application/vnd.americandynamics.acc"
            ".ami", ContentType "application/vnd.amiga.ami"
            ".apk", ContentType "application/vnd.android.package-archive"
            ".cii", ContentType "application/vnd.anser-web-certificate-issue-initiation"
            ".fti", ContentType "application/vnd.anser-web-funds-transfer-initiation"
            ".atx", ContentType "application/vnd.antix.game-component"
            ".mpkg", ContentType "application/vnd.apple.installer+xml"
            ".m3u8", ContentType "application/vnd.apple.mpegurl"
            ".swi", ContentType "application/vnd.aristanetworks.swi"
            ".aep", ContentType "application/vnd.audiograph"
            ".mpm", ContentType "application/vnd.blueice.multipass"
            ".bmi", ContentType "application/vnd.bmi"
            ".rep", ContentType "application/vnd.businessobjects"
            ".cdxml", ContentType "application/vnd.chemdraw+xml"
            ".mmd", ContentType "application/vnd.chipnuts.karaoke-mmd"
            ".cdy", ContentType "application/vnd.cinderella"
            ".cla", ContentType "application/vnd.claymore"
            ".rp9", ContentType "application/vnd.cloanto.rp9"
            ".c4g", ContentType "application/vnd.clonk.c4group"
            ".c11amc", ContentType "application/vnd.cluetrust.cartomobile-config"
            ".c11amz", ContentType "application/vnd.cluetrust.cartomobile-config-pkg"
            ".csp", ContentType "application/vnd.commonspace"
            ".cdbcmsg", ContentType "application/vnd.contact.cmsg"
            ".cmc", ContentType "application/vnd.cosmocaller"
            ".clkx", ContentType "application/vnd.crick.clicker"
            ".clkk", ContentType "application/vnd.crick.clicker.keyboard"
            ".clkp", ContentType "application/vnd.crick.clicker.palette"
            ".clkt", ContentType "application/vnd.crick.clicker.template"
            ".clkw", ContentType "application/vnd.crick.clicker.wordbank"
            ".wbs", ContentType "application/vnd.criticaltools.wbs+xml"
            ".pml", ContentType "application/vnd.ctc-posml"
            ".ppd", ContentType "application/vnd.cups-ppd"
            ".car", ContentType "application/vnd.curl.car"
            ".pcurl", ContentType "application/vnd.curl.pcurl"
            ".rdz", ContentType "application/vnd.data-vision.rdz"
            ".fe_launch", ContentType "application/vnd.denovo.fcselayout-link"
            ".dna", ContentType "application/vnd.dna"
            ".mlp", ContentType "application/vnd.dolby.mlp"
            ".dpg", ContentType "application/vnd.dpgraph"
            ".dfac", ContentType "application/vnd.dreamfactory"
            ".ait", ContentType "application/vnd.dvb.ait"
            ".svc", ContentType "application/vnd.dvb.service"
            ".geo", ContentType "application/vnd.dynageo"
            ".mag", ContentType "application/vnd.ecowin.chart"
            ".nml", ContentType "application/vnd.enliven"
            ".esf", ContentType "application/vnd.epson.esf"
            ".msf", ContentType "application/vnd.epson.msf"
            ".qam", ContentType "application/vnd.epson.quickanime"
            ".slt", ContentType "application/vnd.epson.salt"
            ".ssf", ContentType "application/vnd.epson.ssf"
            ".es3", ContentType "application/vnd.eszigno3+xml"
            ".ez2", ContentType "application/vnd.ezpix-album"
            ".ez3", ContentType "application/vnd.ezpix-package"
            ".fdf", ContentType "application/vnd.fdf"
            ".seed", ContentType "application/vnd.fdsn.seed"
            ".gph", ContentType "application/vnd.flographit"
            ".ftc", ContentType "application/vnd.fluxtime.clip"
            ".fm", ContentType "application/vnd.framemaker"
            ".fnc", ContentType "application/vnd.frogans.fnc"
            ".ltf", ContentType "application/vnd.frogans.ltf"
            ".fsc", ContentType "application/vnd.fsc.weblaunch"
            ".oas", ContentType "application/vnd.fujitsu.oasys"
            ".oa2", ContentType "application/vnd.fujitsu.oasys2"
            ".oa3", ContentType "application/vnd.fujitsu.oasys3"
            ".fg5", ContentType "application/vnd.fujitsu.oasysgp"
            ".bh2", ContentType "application/vnd.fujitsu.oasysprs"
            ".ddd", ContentType "application/vnd.fujixerox.ddd"
            ".xdw", ContentType "application/vnd.fujixerox.docuworks"
            ".xbd", ContentType "application/vnd.fujixerox.docuworks.binder"
            ".fzs", ContentType "application/vnd.fuzzysheet"
            ".txd", ContentType "application/vnd.genomatix.tuxedo"
            ".ggb", ContentType "application/vnd.geogebra.file"
            ".ggt", ContentType "application/vnd.geogebra.tool"
            ".gex", ContentType "application/vnd.geometry-explorer"
            ".gxt", ContentType "application/vnd.geonext"
            ".g2w", ContentType "application/vnd.geoplan"
            ".g3w", ContentType "application/vnd.geospace"
            ".gmx", ContentType "application/vnd.gmx"
            ".kml", ContentType "application/vnd.google-earth.kml+xml"
            ".kmz", ContentType "application/vnd.google-earth.kmz"
            ".gqf", ContentType "application/vnd.grafeq"
            ".gac", ContentType "application/vnd.groove-account"
            ".ghf", ContentType "application/vnd.groove-help"
            ".gim", ContentType "application/vnd.groove-identity-message"
            ".grv", ContentType "application/vnd.groove-injector"
            ".gtm", ContentType "application/vnd.groove-tool-message"
            ".tpl", ContentType "application/vnd.groove-tool-template"
            ".vcg", ContentType "application/vnd.groove-vcard"
            ".hal", ContentType "application/vnd.hal+xml"
            ".zmm", ContentType "application/vnd.handheld-entertainment+xml"
            ".hbci", ContentType "application/vnd.hbci"
            ".les", ContentType "application/vnd.hhe.lesson-player"
            ".hpgl", ContentType "application/vnd.hp-hpgl"
            ".hpid", ContentType "application/vnd.hp-hpid"
            ".hps", ContentType "application/vnd.hp-hps"
            ".jlt", ContentType "application/vnd.hp-jlyt"
            ".pcl", ContentType "application/vnd.hp-pcl"
            ".pclxl", ContentType "application/vnd.hp-pclxl"
            ".sfd-hdstx", ContentType "application/vnd.hydrostatix.sof-data"
            ".x3d", ContentType "application/vnd.hzn-3d-crossword"
            ".mpy", ContentType "application/vnd.ibm.minipay"
            ".afp", ContentType "application/vnd.ibm.modcap"
            ".irm", ContentType "application/vnd.ibm.rights-management"
            ".sc", ContentType "application/vnd.ibm.secure-container"
            ".icc", ContentType "application/vnd.iccprofile"
            ".igl", ContentType "application/vnd.igloader"
            ".ivp", ContentType "application/vnd.immervision-ivp"
            ".ivu", ContentType "application/vnd.immervision-ivu"
            ".igm", ContentType "application/vnd.insors.igm"
            ".xpw", ContentType "application/vnd.intercon.formnet"
            ".i2g", ContentType "application/vnd.intergeo"
            ".qbo", ContentType "application/vnd.intu.qbo"
            ".qfx", ContentType "application/vnd.intu.qfx"
            ".rcprofile", ContentType "application/vnd.ipunplugged.rcprofile"
            ".irp", ContentType "application/vnd.irepository.package+xml"
            ".xpr", ContentType "application/vnd.is-xpr"
            ".fcs", ContentType "application/vnd.isac.fcs"
            ".jam", ContentType "application/vnd.jam"
            ".rms", ContentType "application/vnd.jcp.javame.midlet-rms"
            ".jisp", ContentType "application/vnd.jisp"
            ".joda", ContentType "application/vnd.joost.joda-archive"
            ".ktz", ContentType "application/vnd.kahootz"
            ".karbon", ContentType "application/vnd.kde.karbon"
            ".chrt", ContentType "application/vnd.kde.kchart"
            ".kfo", ContentType "application/vnd.kde.kformula"
            ".flw", ContentType "application/vnd.kde.kivio"
            ".kon", ContentType "application/vnd.kde.kontour"
            ".kpr", ContentType "application/vnd.kde.kpresenter"
            ".ksp", ContentType "application/vnd.kde.kspread"
            ".kwd", ContentType "application/vnd.kde.kword"
            ".htke", ContentType "application/vnd.kenameaapp"
            ".kia", ContentType "application/vnd.kidspiration"
            ".kne", ContentType "application/vnd.kinar"
            ".skp", ContentType "application/vnd.koan"
            ".sse", ContentType "application/vnd.kodak-descriptor"
            ".lasxml", ContentType "application/vnd.las.las+xml"
            ".lbd", ContentType "application/vnd.llamagraphics.life-balance.desktop"
            ".lbe", ContentType "application/vnd.llamagraphics.life-balance.exchange+xml"
            ".123", ContentType "application/vnd.lotus-1-2-3"
            ".apr", ContentType "application/vnd.lotus-approach"
            ".pre", ContentType "application/vnd.lotus-freelance"
            ".nsf", ContentType "application/vnd.lotus-notes"
            ".org", ContentType "application/vnd.lotus-organizer"
            ".scm", ContentType "application/vnd.lotus-screencam"
            ".lwp", ContentType "application/vnd.lotus-wordpro"
            ".portpkg", ContentType "application/vnd.macports.portpkg"
            ".mcd", ContentType "application/vnd.mcd"
            ".mc1", ContentType "application/vnd.medcalcdata"
            ".cdkey", ContentType "application/vnd.mediastation.cdkey"
            ".mwf", ContentType "application/vnd.mfer"
            ".mfm", ContentType "application/vnd.mfmp"
            ".flo", ContentType "application/vnd.micrografx.flo"
            ".igx", ContentType "application/vnd.micrografx.igx"
            ".mif", ContentType "application/vnd.mif"
            ".daf", ContentType "application/vnd.mobius.daf"
            ".dis", ContentType "application/vnd.mobius.dis"
            ".mbk", ContentType "application/vnd.mobius.mbk"
            ".mqy", ContentType "application/vnd.mobius.mqy"
            ".msl", ContentType "application/vnd.mobius.msl"
            ".plc", ContentType "application/vnd.mobius.plc"
            ".txf", ContentType "application/vnd.mobius.txf"
            ".mpn", ContentType "application/vnd.mophun.application"
            ".mpc", ContentType "application/vnd.mophun.certificate"
            ".xul", ContentType "application/vnd.mozilla.xul+xml"
            ".cil", ContentType "application/vnd.ms-artgalry"
            ".cab", ContentType "application/vnd.ms-cab-compressed"
            ".xls", ContentType "application/vnd.ms-excel"
            ".xlam", ContentType "application/vnd.ms-excel.addin.macroenabled.12"
            ".xlsb", ContentType "application/vnd.ms-excel.sheet.binary.macroenabled.12"
            ".xlsm", ContentType "application/vnd.ms-excel.sheet.macroenabled.12"
            ".xltm", ContentType "application/vnd.ms-excel.template.macroenabled.12"
            ".eot", ContentType "application/vnd.ms-fontobject"
            ".chm", ContentType "application/vnd.ms-htmlhelp"
            ".ims", ContentType "application/vnd.ms-ims"
            ".lrm", ContentType "application/vnd.ms-lrm"
            ".thmx", ContentType "application/vnd.ms-officetheme"
            ".cat", ContentType "application/vnd.ms-pki.seccat"
            ".stl", ContentType "application/vnd.ms-pki.stl"
            ".ppt", ContentType "application/vnd.ms-powerpoint"
            ".ppam", ContentType "application/vnd.ms-powerpoint.addin.macroenabled.12"
            ".pptm", ContentType "application/vnd.ms-powerpoint.presentation.macroenabled.12"
            ".sldm", ContentType "application/vnd.ms-powerpoint.slide.macroenabled.12"
            ".ppsm", ContentType "application/vnd.ms-powerpoint.slideshow.macroenabled.12"
            ".potm", ContentType "application/vnd.ms-powerpoint.template.macroenabled.12"
            ".mpp", ContentType "application/vnd.ms-project"
            ".docm", ContentType "application/vnd.ms-word.document.macroenabled.12"
            ".dotm", ContentType "application/vnd.ms-word.template.macroenabled.12"
            ".wps", ContentType "application/vnd.ms-works"
            ".wpl", ContentType "application/vnd.ms-wpl"
            ".xps", ContentType "application/vnd.ms-xpsdocument"
            ".mseq", ContentType "application/vnd.mseq"
            ".mus", ContentType "application/vnd.musician"
            ".msty", ContentType "application/vnd.muvee.style"
            ".nlu", ContentType "application/vnd.neurolanguage.nlu"
            ".nnd", ContentType "application/vnd.noblenet-directory"
            ".nns", ContentType "application/vnd.noblenet-sealer"
            ".nnw", ContentType "application/vnd.noblenet-web"
            ".ngdat", ContentType "application/vnd.nokia.n-gage.data"
            ".n-gage", ContentType "application/vnd.nokia.n-gage.symbian.install"
            ".rpst", ContentType "application/vnd.nokia.radio-preset"
            ".rpss", ContentType "application/vnd.nokia.radio-presets"
            ".edm", ContentType "application/vnd.novadigm.edm"
            ".edx", ContentType "application/vnd.novadigm.edx"
            ".ext", ContentType "application/vnd.novadigm.ext"
            ".odc", ContentType "application/vnd.oasis.opendocument.chart"
            ".otc", ContentType "application/vnd.oasis.opendocument.chart-template"
            ".odb", ContentType "application/vnd.oasis.opendocument.database"
            ".odf", ContentType "application/vnd.oasis.opendocument.formula"
            ".odft", ContentType "application/vnd.oasis.opendocument.formula-template"
            ".odg", ContentType "application/vnd.oasis.opendocument.graphics"
            ".otg", ContentType "application/vnd.oasis.opendocument.graphics-template"
            ".odi", ContentType "application/vnd.oasis.opendocument.image"
            ".oti", ContentType "application/vnd.oasis.opendocument.image-template"
            ".odp", ContentType "application/vnd.oasis.opendocument.presentation"
            ".otp", ContentType "application/vnd.oasis.opendocument.presentation-template"
            ".ods", ContentType "application/vnd.oasis.opendocument.spreadsheet"
            ".ots", ContentType "application/vnd.oasis.opendocument.spreadsheet-template"
            ".odt", ContentType "application/vnd.oasis.opendocument.text"
            ".odm", ContentType "application/vnd.oasis.opendocument.text-master"
            ".ott", ContentType "application/vnd.oasis.opendocument.text-template"
            ".oth", ContentType "application/vnd.oasis.opendocument.text-web"
            ".xo", ContentType "application/vnd.olpc-sugar"
            ".dd2", ContentType "application/vnd.oma.dd2+xml"
            ".oxt", ContentType "application/vnd.openofficeorg.extension"
            ".pptx", ContentType "application/vnd.openxmlformats-officedocument.presentationml.presentation"
            ".sldx", ContentType "application/vnd.openxmlformats-officedocument.presentationml.slide"
            ".ppsx", ContentType "application/vnd.openxmlformats-officedocument.presentationml.slideshow"
            ".potx", ContentType "application/vnd.openxmlformats-officedocument.presentationml.template"
            ".xlsx", ContentType "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            ".xltx", ContentType "application/vnd.openxmlformats-officedocument.spreadsheetml.template"
            ".docx", ContentType "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
            ".dotx", ContentType "application/vnd.openxmlformats-officedocument.wordprocessingml.template"
            ".mgp", ContentType "application/vnd.osgeo.mapguide.package"
            ".dp", ContentType "application/vnd.osgi.dp"
            ".pdb", ContentType "application/vnd.palm"
            ".paw", ContentType "application/vnd.pawaafile"
            ".str", ContentType "application/vnd.pg.format"
            ".ei6", ContentType "application/vnd.pg.osasli"
            ".efif", ContentType "application/vnd.picsel"
            ".wg", ContentType "application/vnd.pmi.widget"
            ".plf", ContentType "application/vnd.pocketlearn"
            ".pbd", ContentType "application/vnd.powerbuilder6"
            ".box", ContentType "application/vnd.previewsystems.box"
            ".mgz", ContentType "application/vnd.proteus.magazine"
            ".qps", ContentType "application/vnd.publishare-delta-tree"
            ".ptid", ContentType "application/vnd.pvi.ptid1"
            ".qxd", ContentType "application/vnd.quark.quarkxpress"
            ".bed", ContentType "application/vnd.realvnc.bed"
            ".mxl", ContentType "application/vnd.recordare.musicxml"
            ".musicxml", ContentType "application/vnd.recordare.musicxml+xml"
            ".cryptonote", ContentType "application/vnd.rig.cryptonote"
            ".cod", ContentType "application/vnd.rim.cod"
            ".rm", ContentType "application/vnd.rn-realmedia"
            ".link66", ContentType "application/vnd.route66.link66+xml"
            ".st", ContentType "application/vnd.sailingtracker.track"
            ".see", ContentType "application/vnd.seemail"
            ".sema", ContentType "application/vnd.sema"
            ".semd", ContentType "application/vnd.semd"
            ".semf", ContentType "application/vnd.semf"
            ".ifm", ContentType "application/vnd.shana.informed.formdata"
            ".itp", ContentType "application/vnd.shana.informed.formtemplate"
            ".iif", ContentType "application/vnd.shana.informed.interchange"
            ".ipk", ContentType "application/vnd.shana.informed.package"
            ".twd", ContentType "application/vnd.simtech-mindmapper"
            ".mmf", ContentType "application/vnd.smaf"
            ".teacher", ContentType "application/vnd.smart.teacher"
            ".sdkm", ContentType "application/vnd.solent.sdkm+xml"
            ".dxp", ContentType "application/vnd.spotfire.dxp"
            ".sfs", ContentType "application/vnd.spotfire.sfs"
            ".sdc", ContentType "application/vnd.stardivision.calc"
            ".sda", ContentType "application/vnd.stardivision.draw"
            ".sdd", ContentType "application/vnd.stardivision.impress"
            ".smf", ContentType "application/vnd.stardivision.math"
            ".sdw", ContentType "application/vnd.stardivision.writer"
            ".sgl", ContentType "application/vnd.stardivision.writer-global"
            ".sm", ContentType "application/vnd.stepmania.stepchart"
            ".sxc", ContentType "application/vnd.sun.xml.calc"
            ".stc", ContentType "application/vnd.sun.xml.calc.template"
            ".sxd", ContentType "application/vnd.sun.xml.draw"
            ".std", ContentType "application/vnd.sun.xml.draw.template"
            ".sxi", ContentType "application/vnd.sun.xml.impress"
            ".sti", ContentType "application/vnd.sun.xml.impress.template"
            ".sxm", ContentType "application/vnd.sun.xml.math"
            ".sxw", ContentType "application/vnd.sun.xml.writer"
            ".sxg", ContentType "application/vnd.sun.xml.writer.global"
            ".stw", ContentType "application/vnd.sun.xml.writer.template"
            ".sus", ContentType "application/vnd.sus-calendar"
            ".svd", ContentType "application/vnd.svd"
            ".sis", ContentType "application/vnd.symbian.install"
            ".xsm", ContentType "application/vnd.syncml+xml"
            ".bdm", ContentType "application/vnd.syncml.dm+wbxml"
            ".xdm", ContentType "application/vnd.syncml.dm+xml"
            ".tao", ContentType "application/vnd.tao.intent-module-archive"
            ".tmo", ContentType "application/vnd.tmobile-livetv"
            ".tpt", ContentType "application/vnd.trid.tpt"
            ".mxs", ContentType "application/vnd.triscape.mxs"
            ".tra", ContentType "application/vnd.trueapp"
            ".ufd", ContentType "application/vnd.ufdl"
            ".utz", ContentType "application/vnd.uiq.theme"
            ".umj", ContentType "application/vnd.umajin"
            ".unityweb", ContentType "application/vnd.unity"
            ".uoml", ContentType "application/vnd.uoml+xml"
            ".vcx", ContentType "application/vnd.vcx"
            ".vsd", ContentType "application/vnd.visio"
            ".vis", ContentType "application/vnd.visionary"
            ".vsf", ContentType "application/vnd.vsf"
            ".wbxml", ContentType "application/vnd.wap.wbxml"
            ".wmlc", ContentType "application/vnd.wap.wmlc"
            ".wmlsc", ContentType "application/vnd.wap.wmlscriptc"
            ".wtb", ContentType "application/vnd.webturbo"
            ".nbp", ContentType "application/vnd.wolfram.player"
            ".wpd", ContentType "application/vnd.wordperfect"
            ".wqd", ContentType "application/vnd.wqd"
            ".stf", ContentType "application/vnd.wt.stf"
            ".xar", ContentType "application/vnd.xara"
            ".xfdl", ContentType "application/vnd.xfdl"
            ".hvd", ContentType "application/vnd.yamaha.hv-dic"
            ".hvs", ContentType "application/vnd.yamaha.hv-script"
            ".hvp", ContentType "application/vnd.yamaha.hv-voice"
            ".osf", ContentType "application/vnd.yamaha.openscoreformat"
            ".osfpvg", ContentType "application/vnd.yamaha.openscoreformat.osfpvg+xml"
            ".saf", ContentType "application/vnd.yamaha.smaf-audio"
            ".spf", ContentType "application/vnd.yamaha.smaf-phrase"
            ".cmp", ContentType "application/vnd.yellowriver-custom-menu"
            ".zir", ContentType "application/vnd.zul"
            ".zaz", ContentType "application/vnd.zzazz.deck+xml"
            ".vxml", ContentType "application/voicexml+xml"
            ".wgt", ContentType "application/widget"
            ".hlp", ContentType "application/winhlp"
            ".wsdl", ContentType "application/wsdl+xml"
            ".wspolicy", ContentType "application/wspolicy+xml"
            ".7z", ContentType "application/x-7z-compressed"
            ".abw", ContentType "application/x-abiword"
            ".ace", ContentType "application/x-ace-compressed"
            ".aab", ContentType "application/x-authorware-bin"
            ".aam", ContentType "application/x-authorware-map"
            ".aas", ContentType "application/x-authorware-seg"
            ".bcpio", ContentType "application/x-bcpio"
            ".torrent", ContentType "application/x-bittorrent"
            ".bz", ContentType "application/x-bzip"
            ".bz2", ContentType "application/x-bzip2"
            ".vcd", ContentType "application/x-cdlink"
            ".chat", ContentType "application/x-chat"
            ".pgn", ContentType "application/x-chess-pgn"
            ".cpio", ContentType "application/x-cpio"
            ".csh", ContentType "application/x-csh"
            ".deb", ContentType "application/x-debian-package"
            ".dir", ContentType "application/x-director"
            ".wad", ContentType "application/x-doom"
            ".ncx", ContentType "application/x-dtbncx+xml"
            ".dtb", ContentType "application/x-dtbook+xml"
            ".res", ContentType "application/x-dtbresource+xml"
            ".dvi", ContentType "application/x-dvi"
            ".bdf", ContentType "application/x-font-bdf"
            ".gsf", ContentType "application/x-font-ghostscript"
            ".psf", ContentType "application/x-font-linux-psf"
            ".otf", ContentType "application/x-font-otf"
            ".pcf", ContentType "application/x-font-pcf"
            ".snf", ContentType "application/x-font-snf"
            ".ttf", ContentType "application/x-font-ttf"
            ".pfa", ContentType "application/x-font-type1"
            ".woff", ContentType "application/x-font-woff"
            ".spl", ContentType "application/x-futuresplash"
            ".gnumeric", ContentType "application/x-gnumeric"
            ".gtar", ContentType "application/x-gtar"
            ".hdf", ContentType "application/x-hdf"
            ".jnlp", ContentType "application/x-java-jnlp-file"
            ".latex", ContentType "application/x-latex"
            ".prc", ContentType "application/x-mobipocket-ebook"
            ".application", ContentType "application/x-ms-application"
            ".wmd", ContentType "application/x-ms-wmd"
            ".wmz", ContentType "application/x-ms-wmz"
            ".xbap", ContentType "application/x-ms-xbap"
            ".mdb", ContentType "application/x-msaccess"
            ".obd", ContentType "application/x-msbinder"
            ".crd", ContentType "application/x-mscardfile"
            ".clp", ContentType "application/x-msclip"
            ".exe", ContentType "application/x-msdownload"
            ".mvb", ContentType "application/x-msmediaview"
            ".wmf", ContentType "application/x-msmetafile"
            ".mny", ContentType "application/x-msmoney"
            ".pub", ContentType "application/x-mspublisher"
            ".scd", ContentType "application/x-msschedule"
            ".trm", ContentType "application/x-msterminal"
            ".wri", ContentType "application/x-mswrite"
            ".nc", ContentType "application/x-netcdf"
            ".p12", ContentType "application/x-pkcs12"
            ".p7b", ContentType "application/x-pkcs7-certificates"
            ".p7r", ContentType "application/x-pkcs7-certreqresp"
            ".rar", ContentType "application/x-rar-compressed"
            ".sh", ContentType "application/x-sh"
            ".shar", ContentType "application/x-shar"
            ".swf", ContentType "application/x-shockwave-flash"
            ".xap", ContentType "application/x-silverlight-app"
            ".sit", ContentType "application/x-stuffit"
            ".sitx", ContentType "application/x-stuffitx"
            ".sv4cpio", ContentType "application/x-sv4cpio"
            ".sv4crc", ContentType "application/x-sv4crc"
            ".tar", ContentType "application/x-tar"
            ".tcl", ContentType "application/x-tcl"
            ".tex", ContentType "application/x-tex"
            ".tfm", ContentType "application/x-tex-tfm"
            ".texinfo", ContentType "application/x-texinfo"
            ".ustar", ContentType "application/x-ustar"
            ".src", ContentType "application/x-wais-source"
            ".der", ContentType "application/x-x509-ca-cert"
            ".fig", ContentType "application/x-xfig"
            ".xpi", ContentType "application/x-xpinstall"
            ".xdf", ContentType "application/xcap-diff+xml"
            ".xenc", ContentType "application/xenc+xml"
            ".xhtml", ContentType "application/xhtml+xml"
            ".xml", ContentType "application/xml"
            ".dtd", ContentType "application/xml-dtd"
            ".xop", ContentType "application/xop+xml"
            ".xslt", ContentType "application/xslt+xml"
            ".xspf", ContentType "application/xspf+xml"
            ".mxml", ContentType "application/xv+xml"
            ".yang", ContentType "application/yang"
            ".yin", ContentType "application/yin+xml"
            ".zip", ContentType "application/zip"
            ".adp", ContentType "audio/adpcm"
            ".au", ContentType "audio/basic"
            ".mid", ContentType "audio/midi"
            ".mp4a", ContentType "audio/mp4"
            ".mpga", ContentType "audio/mpeg"
            ".oga", ContentType "audio/ogg"
            ".uva", ContentType "audio/vnd.dece.audio"
            ".eol", ContentType "audio/vnd.digital-winds"
            ".dra", ContentType "audio/vnd.dra"
            ".dts", ContentType "audio/vnd.dts"
            ".dtshd", ContentType "audio/vnd.dts.hd"
            ".lvp", ContentType "audio/vnd.lucent.voice"
            ".pya", ContentType "audio/vnd.ms-playready.media.pya"
            ".ecelp4800", ContentType "audio/vnd.nuera.ecelp4800"
            ".ecelp7470", ContentType "audio/vnd.nuera.ecelp7470"
            ".ecelp9600", ContentType "audio/vnd.nuera.ecelp9600"
            ".rip", ContentType "audio/vnd.rip"
            ".weba", ContentType "audio/webm"
            ".aac", ContentType "audio/x-aac"
            ".aif", ContentType "audio/x-aiff"
            ".m3u", ContentType "audio/x-mpegurl"
            ".wax", ContentType "audio/x-ms-wax"
            ".wma", ContentType "audio/x-ms-wma"
            ".ram", ContentType "audio/x-pn-realaudio"
            ".rmp", ContentType "audio/x-pn-realaudio-plugin"
            ".wav", ContentType "audio/x-wav"
            ".cdx", ContentType "chemical/x-cdx"
            ".cif", ContentType "chemical/x-cif"
            ".cmdf", ContentType "chemical/x-cmdf"
            ".cml", ContentType "chemical/x-cml"
            ".csml", ContentType "chemical/x-csml"
            ".xyz", ContentType "chemical/x-xyz"
            ".bmp", ContentType "image/bmp"
            ".cgm", ContentType "image/cgm"
            ".g3", ContentType "image/g3fax"
            ".gif", ContentType "image/gif"
            ".ief", ContentType "image/ief"
            ".jpeg, .jpg", ContentType "image/jpeg"
            ".ktx", ContentType "image/ktx"
            ".png", ContentType "image/png"
            ".btif", ContentType "image/prs.btif"
            ".svg", ContentType "image/svg+xml"
            ".tiff", ContentType "image/tiff"
            ".psd", ContentType "image/vnd.adobe.photoshop"
            ".uvi", ContentType "image/vnd.dece.graphic"
            ".sub", ContentType "image/vnd.dvb.subtitle"
            ".djvu", ContentType "image/vnd.djvu"
            ".dwg", ContentType "image/vnd.dwg"
            ".dxf", ContentType "image/vnd.dxf"
            ".fbs", ContentType "image/vnd.fastbidsheet"
            ".fpx", ContentType "image/vnd.fpx"
            ".fst", ContentType "image/vnd.fst"
            ".mmr", ContentType "image/vnd.fujixerox.edmics-mmr"
            ".rlc", ContentType "image/vnd.fujixerox.edmics-rlc"
            ".mdi", ContentType "image/vnd.ms-modi"
            ".npx", ContentType "image/vnd.net-fpx"
            ".wbmp", ContentType "image/vnd.wap.wbmp"
            ".xif", ContentType "image/vnd.xiff"
            ".webp", ContentType "image/webp"
            ".ras", ContentType "image/x-cmu-raster"
            ".cmx", ContentType "image/x-cmx"
            ".fh", ContentType "image/x-freehand"
            ".ico", ContentType "image/x-icon"
            ".pcx", ContentType "image/x-pcx"
            ".pic", ContentType "image/x-pict"
            ".pnm", ContentType "image/x-portable-anymap"
            ".pbm", ContentType "image/x-portable-bitmap"
            ".pgm", ContentType "image/x-portable-graymap"
            ".ppm", ContentType "image/x-portable-pixmap"
            ".rgb", ContentType "image/x-rgb"
            ".xbm", ContentType "image/x-xbitmap"
            ".xpm", ContentType "image/x-xpixmap"
            ".xwd", ContentType "image/x-xwindowdump"
            ".eml", ContentType "message/rfc822"
            ".igs", ContentType "model/iges"
            ".msh", ContentType "model/mesh"
            ".dae", ContentType "model/vnd.collada+xml"
            ".dwf", ContentType "model/vnd.dwf"
            ".gdl", ContentType "model/vnd.gdl"
            ".gtw", ContentType "model/vnd.gtw"
            ".mts", ContentType "model/vnd.mts"
            ".vtu", ContentType "model/vnd.vtu"
            ".wrl", ContentType "model/vrml"
            ".ics", ContentType "text/calendar"
            ".css", ContentType "text/css"
            ".csv", ContentType "text/csv"
            ".html", ContentType "text/html"
            ".n3", ContentType "text/n3"
            ".txt", ContentType "text/plain"
            ".dsc", ContentType "text/prs.lines.tag"
            ".rtx", ContentType "text/richtext"
            ".sgml", ContentType "text/sgml"
            ".tsv", ContentType "text/tab-separated-values"
            ".t", ContentType "text/troff"
            ".ttl", ContentType "text/turtle"
            ".uri", ContentType "text/uri-list"
            ".curl", ContentType "text/vnd.curl"
            ".dcurl", ContentType "text/vnd.curl.dcurl"
            ".scurl", ContentType "text/vnd.curl.scurl"
            ".mcurl", ContentType "text/vnd.curl.mcurl"
            ".fly", ContentType "text/vnd.fly"
            ".flx", ContentType "text/vnd.fmi.flexstor"
            ".gv", ContentType "text/vnd.graphviz"
            ".3dml", ContentType "text/vnd.in3d.3dml"
            ".spot", ContentType "text/vnd.in3d.spot"
            ".jad", ContentType "text/vnd.sun.j2me.app-descriptor"
            ".wml", ContentType "text/vnd.wap.wml"
            ".wmls", ContentType "text/vnd.wap.wmlscript"
            ".s", ContentType "text/x-asm"
            ".c", ContentType "text/x-c"
            ".f", ContentType "text/x-fortran"
            ".p", ContentType "text/x-pascal"
            ".java", ContentType "text/x-java-source,java"
            ".etx", ContentType "text/x-setext"
            ".uu", ContentType "text/x-uuencode"
            ".vcs", ContentType "text/x-vcalendar"
            ".vcf", ContentType "text/x-vcard"
            ".3gp", ContentType "video/3gpp"
            ".3g2", ContentType "video/3gpp2"
            ".h261", ContentType "video/h261"
            ".h263", ContentType "video/h263"
            ".h264", ContentType "video/h264"
            ".jpgv", ContentType "video/jpeg"
            ".jpm", ContentType "video/jpm"
            ".mj2", ContentType "video/mj2"
            ".mp4", ContentType "video/mp4"
            ".mpeg", ContentType "video/mpeg"
            ".ogv", ContentType "video/ogg"
            ".qt", ContentType "video/quicktime"
            ".uvh", ContentType "video/vnd.dece.hd"
            ".uvm", ContentType "video/vnd.dece.mobile"
            ".uvp", ContentType "video/vnd.dece.pd"
            ".uvs", ContentType "video/vnd.dece.sd"
            ".uvv", ContentType "video/vnd.dece.video"
            ".fvt", ContentType "video/vnd.fvt"
            ".mxu", ContentType "video/vnd.mpegurl"
            ".pyv", ContentType "video/vnd.ms-playready.media.pyv"
            ".uvu", ContentType "video/vnd.uvvu.mp4"
            ".viv", ContentType "video/vnd.vivo"
            ".webm", ContentType "video/webm"
            ".f4v", ContentType "video/x-f4v"
            ".fli", ContentType "video/x-fli"
            ".flv", ContentType "video/x-flv"
            ".m4v", ContentType "video/x-m4v"
            ".asf", ContentType "video/x-ms-asf"
            ".wm", ContentType "video/x-ms-wm"
            ".wmv", ContentType "video/x-ms-wmv"
            ".wmx", ContentType "video/x-ms-wmx"
            ".wvx", ContentType "video/x-ms-wvx"
            ".avi", ContentType "video/x-msvideo"
            ".movie", ContentType "video/x-sgi-movie"
            ".ice", ContentType "x-conference/x-cooltalk"
            ".par", ContentType "text/plain-bas"
        |]

    let fromExtension ext = lookupByExtension.TryFind ext

    module Application =
        /// "application/andrew-inset": Andrew Toolkit
        let andrew_inset = ContentType "application/andrew-inset"

        /// "application/applixware": Applixware
        let applixware = ContentType "application/applixware"

        /// "application/atom+xml": Atom Syndication Format
        let atom_xml = ContentType "application/atom+xml"

        /// "application/atomcat+xml": Atom Publishing Protocol
        let atomcat_xml = ContentType "application/atomcat+xml"

        /// "application/atomsvc+xml": Atom Publishing Protocol Service Document
        let atomsvc_xml = ContentType "application/atomsvc+xml"

        /// "application/ccxml+xml,": Voice Browser Call Control
        let ccxml_xml_ = ContentType "application/ccxml+xml,"

        /// "application/cdmi-capability": Cloud Data Management Interface (CDMI) - Capability
        let cdmi_capability = ContentType "application/cdmi-capability"

        /// "application/cdmi-container": Cloud Data Management Interface (CDMI) - Contaimer
        let cdmi_container = ContentType "application/cdmi-container"

        /// "application/cdmi-domain": Cloud Data Management Interface (CDMI) - Domain
        let cdmi_domain = ContentType "application/cdmi-domain"

        /// "application/cdmi-object": Cloud Data Management Interface (CDMI) - Object
        let cdmi_object = ContentType "application/cdmi-object"

        /// "application/cdmi-queue": Cloud Data Management Interface (CDMI) - Queue
        let cdmi_queue = ContentType "application/cdmi-queue"

        /// "application/cu-seeme": CU-SeeMe
        let cu_seeme = ContentType "application/cu-seeme"

        /// "application/davmount+xml": Web Distributed Authoring and Versioning
        let davmount_xml = ContentType "application/davmount+xml"

        /// "application/dssc+der": Data Structure for the Security Suitability of Cryptographic Algorithms
        let dssc_der = ContentType "application/dssc+der"

        /// "application/dssc+xml": Data Structure for the Security Suitability of Cryptographic Algorithms
        let dssc_xml = ContentType "application/dssc+xml"

        /// "application/ecmascript": ECMAScript
        let ecmascript = ContentType "application/ecmascript"

        /// "application/emma+xml": Extensible MultiModal Annotation
        let emma_xml = ContentType "application/emma+xml"

        /// "application/epub+zip": Electronic Publication
        let epub_zip = ContentType "application/epub+zip"

        /// "application/exi": Efficient XML Interchange
        let exi = ContentType "application/exi"

        /// "application/font-tdpfr": Portable Font Resource
        let font_tdpfr = ContentType "application/font-tdpfr"

        /// "application/hyperstudio": Hyperstudio
        let hyperstudio = ContentType "application/hyperstudio"

        /// "application/ipfix": Internet Protocol Flow Information Export
        let ipfix = ContentType "application/ipfix"

        /// "application/java-archive": Java Archive
        let java_archive = ContentType "application/java-archive"

        /// "application/java-serialized-object": Java Serialized Object
        let java_serialized_object = ContentType "application/java-serialized-object"

        /// "application/java-vm": Java Bytecode File
        let java_vm = ContentType "application/java-vm"

        /// "application/javascript": JavaScript
        let javascript = ContentType "application/javascript"

        /// "application/json": JavaScript Object Notation (JSON)
        let json = ContentType "application/json"

        /// "application/mads+xml": Metadata Authority  Description Schema
        let mads_xml = ContentType "application/mads+xml"

        /// "application/marc": MARC Formats
        let marc = ContentType "application/marc"

        /// "application/marcxml+xml": MARC21 XML Schema
        let marcxml_xml = ContentType "application/marcxml+xml"

        /// "application/mathematica": Mathematica Notebooks
        let mathematica = ContentType "application/mathematica"

        /// "application/mathml+xml": Mathematical Markup Language
        let mathml_xml = ContentType "application/mathml+xml"

        /// "application/mbox": Mbox database files
        let mbox = ContentType "application/mbox"

        /// "application/mediaservercontrol+xml": Media Server Control Markup Language
        let mediaservercontrol_xml = ContentType "application/mediaservercontrol+xml"

        /// "application/metalink4+xml": Metalink
        let metalink4_xml = ContentType "application/metalink4+xml"

        /// "application/mets+xml": Metadata Encoding and Transmission Standard
        let mets_xml = ContentType "application/mets+xml"

        /// "application/mods+xml": Metadata Object Description Schema
        let mods_xml = ContentType "application/mods+xml"

        /// "application/mp21": MPEG-21
        let mp21 = ContentType "application/mp21"

        /// "application/mp4": MPEG4
        let mp4 = ContentType "application/mp4"

        /// "application/msword": Microsoft Word
        let msword = ContentType "application/msword"

        /// "application/mxf": Material Exchange Format
        let mxf = ContentType "application/mxf"

        /// "application/octet-stream": Binary Data
        let octet_stream = ContentType "application/octet-stream"

        /// "application/oda": Office Document Architecture
        let oda = ContentType "application/oda"

        /// "application/oebps-package+xml": Open eBook Publication Structure
        let oebps_package_xml = ContentType "application/oebps-package+xml"

        /// "application/ogg": Ogg
        let ogg = ContentType "application/ogg"

        /// "application/onenote": Microsoft OneNote
        let onenote = ContentType "application/onenote"

        /// "application/patch-ops-error+xml": XML Patch Framework
        let patch_ops_error_xml = ContentType "application/patch-ops-error+xml"

        /// "application/pdf": Adobe Portable Document Format
        let pdf = ContentType "application/pdf"

        /// "application/pgp-encrypted": Pretty Good Privacy
        let pgp_encrypted = ContentType "application/pgp-encrypted"

        /// "application/pgp-signature": Pretty Good Privacy - Signature
        let pgp_signature = ContentType "application/pgp-signature"

        /// "application/pics-rules": PICSRules
        let pics_rules = ContentType "application/pics-rules"

        /// "application/pkcs10": PKCS #10 - Certification Request Standard
        let pkcs10 = ContentType "application/pkcs10"

        /// "application/pkcs7-mime": PKCS #7 - Cryptographic Message Syntax Standard
        let pkcs7_mime = ContentType "application/pkcs7-mime"

        /// "application/pkcs7-signature": PKCS #7 - Cryptographic Message Syntax Standard
        let pkcs7_signature = ContentType "application/pkcs7-signature"

        /// "application/pkcs8": PKCS #8 - Private-Key Information Syntax Standard
        let pkcs8 = ContentType "application/pkcs8"

        /// "application/pkix-attr-cert": Attribute Certificate
        let pkix_attr_cert = ContentType "application/pkix-attr-cert"

        /// "application/pkix-cert": Internet Public Key Infrastructure - Certificate
        let pkix_cert = ContentType "application/pkix-cert"

        /// "application/pkix-crl": Internet Public Key Infrastructure - Certificate Revocation Lists
        let pkix_crl = ContentType "application/pkix-crl"

        /// "application/pkix-pkipath": Internet Public Key Infrastructure - Certification Path
        let pkix_pkipath = ContentType "application/pkix-pkipath"

        /// "application/pkixcmp": Internet Public Key Infrastructure - Certificate Management Protocole
        let pkixcmp = ContentType "application/pkixcmp"

        /// "application/pls+xml": Pronunciation Lexicon Specification
        let pls_xml = ContentType "application/pls+xml"

        /// "application/postscript": PostScript
        let postscript = ContentType "application/postscript"

        /// "application/prs.cww": CU-Writer
        let prs_cww = ContentType "application/prs.cww"

        /// "application/pskc+xml": Portable Symmetric Key Container
        let pskc_xml = ContentType "application/pskc+xml"

        /// "application/rdf+xml": Resource Description Framework
        let rdf_xml = ContentType "application/rdf+xml"

        /// "application/reginfo+xml": IMS Networks
        let reginfo_xml = ContentType "application/reginfo+xml"

        /// "application/relax-ng-compact-syntax": Relax NG Compact Syntax
        let relax_ng_compact_syntax = ContentType "application/relax-ng-compact-syntax"

        /// "application/resource-lists+xml": XML Resource Lists
        let resource_lists_xml = ContentType "application/resource-lists+xml"

        /// "application/resource-lists-diff+xml": XML Resource Lists Diff
        let resource_lists_diff_xml = ContentType "application/resource-lists-diff+xml"

        /// "application/rls-services+xml": XML Resource Lists
        let rls_services_xml = ContentType "application/rls-services+xml"

        /// "application/rsd+xml": Really Simple Discovery
        let rsd_xml = ContentType "application/rsd+xml"

        /// "application/rss+xml": RSS - Really Simple Syndication
        let rss_xml = ContentType "application/rss+xml"

        /// "application/rtf": Rich Text Format
        let rtf = ContentType "application/rtf"

        /// "application/sbml+xml": Systems Biology Markup Language
        let sbml_xml = ContentType "application/sbml+xml"

        /// "application/scvp-cv-request": Server-Based Certificate Validation Protocol - Validation Request
        let scvp_cv_request = ContentType "application/scvp-cv-request"

        /// "application/scvp-cv-response": Server-Based Certificate Validation Protocol - Validation Response
        let scvp_cv_response = ContentType "application/scvp-cv-response"

        /// "application/scvp-vp-request": Server-Based Certificate Validation Protocol - Validation Policies - Request
        let scvp_vp_request = ContentType "application/scvp-vp-request"

        /// "application/scvp-vp-response": Server-Based Certificate Validation Protocol - Validation Policies - Response
        let scvp_vp_response = ContentType "application/scvp-vp-response"

        /// "application/sdp": Session Description Protocol
        let sdp = ContentType "application/sdp"

        /// "application/set-payment-initiation": Secure Electronic Transaction - Payment
        let set_payment_initiation = ContentType "application/set-payment-initiation"

        /// "application/set-registration-initiation": Secure Electronic Transaction - Registration
        let set_registration_initiation = ContentType "application/set-registration-initiation"

        /// "application/shf+xml": S Hexdump Format
        let shf_xml = ContentType "application/shf+xml"

        /// "application/smil+xml": Synchronized Multimedia Integration Language
        let smil_xml = ContentType "application/smil+xml"

        /// "application/sparql-query": SPARQL - Query
        let sparql_query = ContentType "application/sparql-query"

        /// "application/sparql-results+xml": SPARQL - Results
        let sparql_results_xml = ContentType "application/sparql-results+xml"

        /// "application/srgs": Speech Recognition Grammar Specification
        let srgs = ContentType "application/srgs"

        /// "application/srgs+xml": Speech Recognition Grammar Specification - XML
        let srgs_xml = ContentType "application/srgs+xml"

        /// "application/sru+xml": Search/Retrieve via URL Response Format
        let sru_xml = ContentType "application/sru+xml"

        /// "application/ssml+xml": Speech Synthesis Markup Language
        let ssml_xml = ContentType "application/ssml+xml"

        /// "application/tei+xml": Text Encoding and Interchange
        let tei_xml = ContentType "application/tei+xml"

        /// "application/thraud+xml": Sharing Transaction Fraud Data
        let thraud_xml = ContentType "application/thraud+xml"

        /// "application/timestamped-data": Time Stamped Data Envelope
        let timestamped_data = ContentType "application/timestamped-data"

        /// "application/vnd.3gpp.pic-bw-large": 3rd Generation Partnership Project - Pic Large
        let vnd_3gpp_pic_bw_large = ContentType "application/vnd.3gpp.pic-bw-large"

        /// "application/vnd.3gpp.pic-bw-small": 3rd Generation Partnership Project - Pic Small
        let vnd_3gpp_pic_bw_small = ContentType "application/vnd.3gpp.pic-bw-small"

        /// "application/vnd.3gpp.pic-bw-var": 3rd Generation Partnership Project - Pic Var
        let vnd_3gpp_pic_bw_var = ContentType "application/vnd.3gpp.pic-bw-var"

        /// "application/vnd.3gpp2.tcap": 3rd Generation Partnership Project - Transaction Capabilities Application Part
        let vnd_3gpp2_tcap = ContentType "application/vnd.3gpp2.tcap"

        /// "application/vnd.3m.post-it-notes": 3M Post It Notes
        let vnd_3m_post_it_notes = ContentType "application/vnd.3m.post-it-notes"

        /// "application/vnd.accpac.simply.aso": Simply Accounting
        let vnd_accpac_simply_aso = ContentType "application/vnd.accpac.simply.aso"

        /// "application/vnd.accpac.simply.imp": Simply Accounting - Data Import
        let vnd_accpac_simply_imp = ContentType "application/vnd.accpac.simply.imp"

        /// "application/vnd.acucobol": ACU Cobol
        let vnd_acucobol = ContentType "application/vnd.acucobol"

        /// "application/vnd.acucorp": ACU Cobol
        let vnd_acucorp = ContentType "application/vnd.acucorp"

        /// "application/vnd.adobe.air-application-installer-package+zip": Adobe AIR Application
        let vnd_adobe_air_application_installer_package_zip = ContentType "application/vnd.adobe.air-application-installer-package+zip"

        /// "application/vnd.adobe.fxp": Adobe Flex Project
        let vnd_adobe_fxp = ContentType "application/vnd.adobe.fxp"

        /// "application/vnd.adobe.xdp+xml": Adobe XML Data Package
        let vnd_adobe_xdp_xml = ContentType "application/vnd.adobe.xdp+xml"

        /// "application/vnd.adobe.xfdf": Adobe XML Forms Data Format
        let vnd_adobe_xfdf = ContentType "application/vnd.adobe.xfdf"

        /// "application/vnd.ahead.space": Ahead AIR Application
        let vnd_ahead_space = ContentType "application/vnd.ahead.space"

        /// "application/vnd.airzip.filesecure.azf": AirZip FileSECURE
        let vnd_airzip_filesecure_azf = ContentType "application/vnd.airzip.filesecure.azf"

        /// "application/vnd.airzip.filesecure.azs": AirZip FileSECURE
        let vnd_airzip_filesecure_azs = ContentType "application/vnd.airzip.filesecure.azs"

        /// "application/vnd.amazon.ebook": Amazon Kindle eBook format
        let vnd_amazon_ebook = ContentType "application/vnd.amazon.ebook"

        /// "application/vnd.americandynamics.acc": Active Content Compression
        let vnd_americandynamics_acc = ContentType "application/vnd.americandynamics.acc"

        /// "application/vnd.amiga.ami": AmigaDE
        let vnd_amiga_ami = ContentType "application/vnd.amiga.ami"

        /// "application/vnd.android.package-archive": Android Package Archive
        let vnd_android_package_archive = ContentType "application/vnd.android.package-archive"

        /// "application/vnd.anser-web-certificate-issue-initiation": ANSER-WEB Terminal Client - Certificate Issue
        let vnd_anser_web_certificate_issue_initiation = ContentType "application/vnd.anser-web-certificate-issue-initiation"

        /// "application/vnd.anser-web-funds-transfer-initiation": ANSER-WEB Terminal Client - Web Funds Transfer
        let vnd_anser_web_funds_transfer_initiation = ContentType "application/vnd.anser-web-funds-transfer-initiation"

        /// "application/vnd.antix.game-component": Antix Game Player
        let vnd_antix_game_component = ContentType "application/vnd.antix.game-component"

        /// "application/vnd.apple.installer+xml": Apple Installer Package
        let vnd_apple_installer_xml = ContentType "application/vnd.apple.installer+xml"

        /// "application/vnd.apple.mpegurl": Multimedia Playlist Unicode
        let vnd_apple_mpegurl = ContentType "application/vnd.apple.mpegurl"

        /// "application/vnd.aristanetworks.swi": Arista Networks Software Image
        let vnd_aristanetworks_swi = ContentType "application/vnd.aristanetworks.swi"

        /// "application/vnd.audiograph": Audiograph
        let vnd_audiograph = ContentType "application/vnd.audiograph"

        /// "application/vnd.blueice.multipass": Blueice Research Multipass
        let vnd_blueice_multipass = ContentType "application/vnd.blueice.multipass"

        /// "application/vnd.bmi": BMI Drawing Data Interchange
        let vnd_bmi = ContentType "application/vnd.bmi"

        /// "application/vnd.businessobjects": BusinessObjects
        let vnd_businessobjects = ContentType "application/vnd.businessobjects"

        /// "application/vnd.chemdraw+xml": CambridgeSoft Chem Draw
        let vnd_chemdraw_xml = ContentType "application/vnd.chemdraw+xml"

        /// "application/vnd.chipnuts.karaoke-mmd": Karaoke on Chipnuts Chipsets
        let vnd_chipnuts_karaoke_mmd = ContentType "application/vnd.chipnuts.karaoke-mmd"

        /// "application/vnd.cinderella": Interactive Geometry Software Cinderella
        let vnd_cinderella = ContentType "application/vnd.cinderella"

        /// "application/vnd.claymore": Claymore Data Files
        let vnd_claymore = ContentType "application/vnd.claymore"

        /// "application/vnd.cloanto.rp9": RetroPlatform Player
        let vnd_cloanto_rp9 = ContentType "application/vnd.cloanto.rp9"

        /// "application/vnd.clonk.c4group": Clonk Game
        let vnd_clonk_c4group = ContentType "application/vnd.clonk.c4group"

        /// "application/vnd.cluetrust.cartomobile-config": ClueTrust CartoMobile - Config
        let vnd_cluetrust_cartomobile_config = ContentType "application/vnd.cluetrust.cartomobile-config"

        /// "application/vnd.cluetrust.cartomobile-config-pkg": ClueTrust CartoMobile - Config Package
        let vnd_cluetrust_cartomobile_config_pkg = ContentType "application/vnd.cluetrust.cartomobile-config-pkg"

        /// "application/vnd.commonspace": Sixth Floor Media - CommonSpace
        let vnd_commonspace = ContentType "application/vnd.commonspace"

        /// "application/vnd.contact.cmsg": CIM Database
        let vnd_contact_cmsg = ContentType "application/vnd.contact.cmsg"

        /// "application/vnd.cosmocaller": CosmoCaller
        let vnd_cosmocaller = ContentType "application/vnd.cosmocaller"

        /// "application/vnd.crick.clicker": CrickSoftware - Clicker
        let vnd_crick_clicker = ContentType "application/vnd.crick.clicker"

        /// "application/vnd.crick.clicker.keyboard": CrickSoftware - Clicker - Keyboard
        let vnd_crick_clicker_keyboard = ContentType "application/vnd.crick.clicker.keyboard"

        /// "application/vnd.crick.clicker.palette": CrickSoftware - Clicker - Palette
        let vnd_crick_clicker_palette = ContentType "application/vnd.crick.clicker.palette"

        /// "application/vnd.crick.clicker.template": CrickSoftware - Clicker - Template
        let vnd_crick_clicker_template = ContentType "application/vnd.crick.clicker.template"

        /// "application/vnd.crick.clicker.wordbank": CrickSoftware - Clicker - Wordbank
        let vnd_crick_clicker_wordbank = ContentType "application/vnd.crick.clicker.wordbank"

        /// "application/vnd.criticaltools.wbs+xml": Critical Tools - PERT Chart EXPERT
        let vnd_criticaltools_wbs_xml = ContentType "application/vnd.criticaltools.wbs+xml"

        /// "application/vnd.ctc-posml": PosML
        let vnd_ctc_posml = ContentType "application/vnd.ctc-posml"

        /// "application/vnd.cups-ppd": Adobe PostScript Printer Description File Format
        let vnd_cups_ppd = ContentType "application/vnd.cups-ppd"

        /// "application/vnd.curl.car": CURL Applet
        let vnd_curl_car = ContentType "application/vnd.curl.car"

        /// "application/vnd.curl.pcurl": CURL Applet
        let vnd_curl_pcurl = ContentType "application/vnd.curl.pcurl"

        /// "application/vnd.data-vision.rdz": RemoteDocs R-Viewer
        let vnd_data_vision_rdz = ContentType "application/vnd.data-vision.rdz"

        /// "application/vnd.denovo.fcselayout-link": FCS Express Layout Link
        let vnd_denovo_fcselayout_link = ContentType "application/vnd.denovo.fcselayout-link"

        /// "application/vnd.dna": New Moon Liftoff/DNA
        let vnd_dna = ContentType "application/vnd.dna"

        /// "application/vnd.dolby.mlp": Dolby Meridian Lossless Packing
        let vnd_dolby_mlp = ContentType "application/vnd.dolby.mlp"

        /// "application/vnd.dpgraph": DPGraph
        let vnd_dpgraph = ContentType "application/vnd.dpgraph"

        /// "application/vnd.dreamfactory": DreamFactory
        let vnd_dreamfactory = ContentType "application/vnd.dreamfactory"

        /// "application/vnd.dvb.ait": Digital Video Broadcasting
        let vnd_dvb_ait = ContentType "application/vnd.dvb.ait"

        /// "application/vnd.dvb.service": Digital Video Broadcasting
        let vnd_dvb_service = ContentType "application/vnd.dvb.service"

        /// "application/vnd.dynageo": DynaGeo
        let vnd_dynageo = ContentType "application/vnd.dynageo"

        /// "application/vnd.ecowin.chart": EcoWin Chart
        let vnd_ecowin_chart = ContentType "application/vnd.ecowin.chart"

        /// "application/vnd.enliven": Enliven Viewer
        let vnd_enliven = ContentType "application/vnd.enliven"

        /// "application/vnd.epson.esf": QUASS Stream Player
        let vnd_epson_esf = ContentType "application/vnd.epson.esf"

        /// "application/vnd.epson.msf": QUASS Stream Player
        let vnd_epson_msf = ContentType "application/vnd.epson.msf"

        /// "application/vnd.epson.quickanime": QuickAnime Player
        let vnd_epson_quickanime = ContentType "application/vnd.epson.quickanime"

        /// "application/vnd.epson.salt": SimpleAnimeLite Player
        let vnd_epson_salt = ContentType "application/vnd.epson.salt"

        /// "application/vnd.epson.ssf": QUASS Stream Player
        let vnd_epson_ssf = ContentType "application/vnd.epson.ssf"

        /// "application/vnd.eszigno3+xml": MICROSEC e-Szign¢
        let vnd_eszigno3_xml = ContentType "application/vnd.eszigno3+xml"

        /// "application/vnd.ezpix-album": EZPix Secure Photo Album
        let vnd_ezpix_album = ContentType "application/vnd.ezpix-album"

        /// "application/vnd.ezpix-package": EZPix Secure Photo Album
        let vnd_ezpix_package = ContentType "application/vnd.ezpix-package"

        /// "application/vnd.fdf": Forms Data Format
        let vnd_fdf = ContentType "application/vnd.fdf"

        /// "application/vnd.fdsn.seed": Digital Siesmograph Networks - SEED Datafiles
        let vnd_fdsn_seed = ContentType "application/vnd.fdsn.seed"

        /// "application/vnd.flographit": NpGraphIt
        let vnd_flographit = ContentType "application/vnd.flographit"

        /// "application/vnd.fluxtime.clip": FluxTime Clip
        let vnd_fluxtime_clip = ContentType "application/vnd.fluxtime.clip"

        /// "application/vnd.framemaker": FrameMaker Normal Format
        let vnd_framemaker = ContentType "application/vnd.framemaker"

        /// "application/vnd.frogans.fnc": Frogans Player
        let vnd_frogans_fnc = ContentType "application/vnd.frogans.fnc"

        /// "application/vnd.frogans.ltf": Frogans Player
        let vnd_frogans_ltf = ContentType "application/vnd.frogans.ltf"

        /// "application/vnd.fsc.weblaunch": Friendly Software Corporation
        let vnd_fsc_weblaunch = ContentType "application/vnd.fsc.weblaunch"

        /// "application/vnd.fujitsu.oasys": Fujitsu Oasys
        let vnd_fujitsu_oasys = ContentType "application/vnd.fujitsu.oasys"

        /// "application/vnd.fujitsu.oasys2": Fujitsu Oasys
        let vnd_fujitsu_oasys2 = ContentType "application/vnd.fujitsu.oasys2"

        /// "application/vnd.fujitsu.oasys3": Fujitsu Oasys
        let vnd_fujitsu_oasys3 = ContentType "application/vnd.fujitsu.oasys3"

        /// "application/vnd.fujitsu.oasysgp": Fujitsu Oasys
        let vnd_fujitsu_oasysgp = ContentType "application/vnd.fujitsu.oasysgp"

        /// "application/vnd.fujitsu.oasysprs": Fujitsu Oasys
        let vnd_fujitsu_oasysprs = ContentType "application/vnd.fujitsu.oasysprs"

        /// "application/vnd.fujixerox.ddd": Fujitsu - Xerox 2D CAD Data
        let vnd_fujixerox_ddd = ContentType "application/vnd.fujixerox.ddd"

        /// "application/vnd.fujixerox.docuworks": Fujitsu - Xerox DocuWorks
        let vnd_fujixerox_docuworks = ContentType "application/vnd.fujixerox.docuworks"

        /// "application/vnd.fujixerox.docuworks.binder": Fujitsu - Xerox DocuWorks Binder
        let vnd_fujixerox_docuworks_binder = ContentType "application/vnd.fujixerox.docuworks.binder"

        /// "application/vnd.fuzzysheet": FuzzySheet
        let vnd_fuzzysheet = ContentType "application/vnd.fuzzysheet"

        /// "application/vnd.genomatix.tuxedo": Genomatix Tuxedo Framework
        let vnd_genomatix_tuxedo = ContentType "application/vnd.genomatix.tuxedo"

        /// "application/vnd.geogebra.file": GeoGebra
        let vnd_geogebra_file = ContentType "application/vnd.geogebra.file"

        /// "application/vnd.geogebra.tool": GeoGebra
        let vnd_geogebra_tool = ContentType "application/vnd.geogebra.tool"

        /// "application/vnd.geometry-explorer": GeoMetry Explorer
        let vnd_geometry_explorer = ContentType "application/vnd.geometry-explorer"

        /// "application/vnd.geonext": GEONExT and JSXGraph
        let vnd_geonext = ContentType "application/vnd.geonext"

        /// "application/vnd.geoplan": GeoplanW
        let vnd_geoplan = ContentType "application/vnd.geoplan"

        /// "application/vnd.geospace": GeospacW
        let vnd_geospace = ContentType "application/vnd.geospace"

        /// "application/vnd.gmx": GameMaker ActiveX
        let vnd_gmx = ContentType "application/vnd.gmx"

        /// "application/vnd.google-earth.kml+xml": Google Earth - KML
        let vnd_google_earth_kml_xml = ContentType "application/vnd.google-earth.kml+xml"

        /// "application/vnd.google-earth.kmz": Google Earth - Zipped KML
        let vnd_google_earth_kmz = ContentType "application/vnd.google-earth.kmz"

        /// "application/vnd.grafeq": GrafEq
        let vnd_grafeq = ContentType "application/vnd.grafeq"

        /// "application/vnd.groove-account": Groove - Account
        let vnd_groove_account = ContentType "application/vnd.groove-account"

        /// "application/vnd.groove-help": Groove - Help
        let vnd_groove_help = ContentType "application/vnd.groove-help"

        /// "application/vnd.groove-identity-message": Groove - Identity Message
        let vnd_groove_identity_message = ContentType "application/vnd.groove-identity-message"

        /// "application/vnd.groove-injector": Groove - Injector
        let vnd_groove_injector = ContentType "application/vnd.groove-injector"

        /// "application/vnd.groove-tool-message": Groove - Tool Message
        let vnd_groove_tool_message = ContentType "application/vnd.groove-tool-message"

        /// "application/vnd.groove-tool-template": Groove - Tool Template
        let vnd_groove_tool_template = ContentType "application/vnd.groove-tool-template"

        /// "application/vnd.groove-vcard": Groove - Vcard
        let vnd_groove_vcard = ContentType "application/vnd.groove-vcard"

        /// "application/vnd.hal+xml": Hypertext Application Language
        let vnd_hal_xml = ContentType "application/vnd.hal+xml"

        /// "application/vnd.handheld-entertainment+xml": ZVUE Media Manager
        let vnd_handheld_entertainment_xml = ContentType "application/vnd.handheld-entertainment+xml"

        /// "application/vnd.hbci": Homebanking Computer Interface (HBCI)
        let vnd_hbci = ContentType "application/vnd.hbci"

        /// "application/vnd.hhe.lesson-player": Archipelago Lesson Player
        let vnd_hhe_lesson_player = ContentType "application/vnd.hhe.lesson-player"

        /// "application/vnd.hp-hpgl": HP-GL/2 and HP RTL
        let vnd_hp_hpgl = ContentType "application/vnd.hp-hpgl"

        /// "application/vnd.hp-hpid": Hewlett Packard Instant Delivery
        let vnd_hp_hpid = ContentType "application/vnd.hp-hpid"

        /// "application/vnd.hp-hps": Hewlett-Packard's WebPrintSmart
        let vnd_hp_hps = ContentType "application/vnd.hp-hps"

        /// "application/vnd.hp-jlyt": HP Indigo Digital Press - Job Layout Languate
        let vnd_hp_jlyt = ContentType "application/vnd.hp-jlyt"

        /// "application/vnd.hp-pcl": HP Printer Command Language
        let vnd_hp_pcl = ContentType "application/vnd.hp-pcl"

        /// "application/vnd.hp-pclxl": PCL 6 Enhanced (Formely PCL XL)
        let vnd_hp_pclxl = ContentType "application/vnd.hp-pclxl"

        /// "application/vnd.hydrostatix.sof-data": Hydrostatix Master Suite
        let vnd_hydrostatix_sof_data = ContentType "application/vnd.hydrostatix.sof-data"

        /// "application/vnd.hzn-3d-crossword": 3D Crossword Plugin
        let vnd_hzn_3d_crossword = ContentType "application/vnd.hzn-3d-crossword"

        /// "application/vnd.ibm.minipay": MiniPay
        let vnd_ibm_minipay = ContentType "application/vnd.ibm.minipay"

        /// "application/vnd.ibm.modcap": MO:DCA-P
        let vnd_ibm_modcap = ContentType "application/vnd.ibm.modcap"

        /// "application/vnd.ibm.rights-management": IBM DB2 Rights Manager
        let vnd_ibm_rights_management = ContentType "application/vnd.ibm.rights-management"

        /// "application/vnd.ibm.secure-container": IBM Electronic Media Management System - Secure Container
        let vnd_ibm_secure_container = ContentType "application/vnd.ibm.secure-container"

        /// "application/vnd.iccprofile": ICC profile
        let vnd_iccprofile = ContentType "application/vnd.iccprofile"

        /// "application/vnd.igloader": igLoader
        let vnd_igloader = ContentType "application/vnd.igloader"

        /// "application/vnd.immervision-ivp": ImmerVision PURE Players
        let vnd_immervision_ivp = ContentType "application/vnd.immervision-ivp"

        /// "application/vnd.immervision-ivu": ImmerVision PURE Players
        let vnd_immervision_ivu = ContentType "application/vnd.immervision-ivu"

        /// "application/vnd.insors.igm": IOCOM Visimeet
        let vnd_insors_igm = ContentType "application/vnd.insors.igm"

        /// "application/vnd.intercon.formnet": Intercon FormNet
        let vnd_intercon_formnet = ContentType "application/vnd.intercon.formnet"

        /// "application/vnd.intergeo": Interactive Geometry Software
        let vnd_intergeo = ContentType "application/vnd.intergeo"

        /// "application/vnd.intu.qbo": Open Financial Exchange
        let vnd_intu_qbo = ContentType "application/vnd.intu.qbo"

        /// "application/vnd.intu.qfx": Quicken
        let vnd_intu_qfx = ContentType "application/vnd.intu.qfx"

        /// "application/vnd.ipunplugged.rcprofile": IP Unplugged Roaming Client
        let vnd_ipunplugged_rcprofile = ContentType "application/vnd.ipunplugged.rcprofile"

        /// "application/vnd.irepository.package+xml": iRepository / Lucidoc Editor
        let vnd_irepository_package_xml = ContentType "application/vnd.irepository.package+xml"

        /// "application/vnd.is-xpr": Express by Infoseek
        let vnd_is_xpr = ContentType "application/vnd.is-xpr"

        /// "application/vnd.isac.fcs": International Society for Advancement of Cytometry
        let vnd_isac_fcs = ContentType "application/vnd.isac.fcs"

        /// "application/vnd.jam": Lightspeed Audio Lab
        let vnd_jam = ContentType "application/vnd.jam"

        /// "application/vnd.jcp.javame.midlet-rms": Mobile Information Device Profile
        let vnd_jcp_javame_midlet_rms = ContentType "application/vnd.jcp.javame.midlet-rms"

        /// "application/vnd.jisp": RhymBox
        let vnd_jisp = ContentType "application/vnd.jisp"

        /// "application/vnd.joost.joda-archive": Joda Archive
        let vnd_joost_joda_archive = ContentType "application/vnd.joost.joda-archive"

        /// "application/vnd.kahootz": Kahootz
        let vnd_kahootz = ContentType "application/vnd.kahootz"

        /// "application/vnd.kde.karbon": KDE KOffice Office Suite - Karbon
        let vnd_kde_karbon = ContentType "application/vnd.kde.karbon"

        /// "application/vnd.kde.kchart": KDE KOffice Office Suite - KChart
        let vnd_kde_kchart = ContentType "application/vnd.kde.kchart"

        /// "application/vnd.kde.kformula": KDE KOffice Office Suite - Kformula
        let vnd_kde_kformula = ContentType "application/vnd.kde.kformula"

        /// "application/vnd.kde.kivio": KDE KOffice Office Suite - Kivio
        let vnd_kde_kivio = ContentType "application/vnd.kde.kivio"

        /// "application/vnd.kde.kontour": KDE KOffice Office Suite - Kontour
        let vnd_kde_kontour = ContentType "application/vnd.kde.kontour"

        /// "application/vnd.kde.kpresenter": KDE KOffice Office Suite - Kpresenter
        let vnd_kde_kpresenter = ContentType "application/vnd.kde.kpresenter"

        /// "application/vnd.kde.kspread": KDE KOffice Office Suite - Kspread
        let vnd_kde_kspread = ContentType "application/vnd.kde.kspread"

        /// "application/vnd.kde.kword": KDE KOffice Office Suite - Kword
        let vnd_kde_kword = ContentType "application/vnd.kde.kword"

        /// "application/vnd.kenameaapp": Kenamea App
        let vnd_kenameaapp = ContentType "application/vnd.kenameaapp"

        /// "application/vnd.kidspiration": Kidspiration
        let vnd_kidspiration = ContentType "application/vnd.kidspiration"

        /// "application/vnd.kinar": Kinar Applications
        let vnd_kinar = ContentType "application/vnd.kinar"

        /// "application/vnd.koan": SSEYO Koan Play File
        let vnd_koan = ContentType "application/vnd.koan"

        /// "application/vnd.kodak-descriptor": Kodak Storyshare
        let vnd_kodak_descriptor = ContentType "application/vnd.kodak-descriptor"

        /// "application/vnd.las.las+xml": Laser App Enterprise
        let vnd_las_las_xml = ContentType "application/vnd.las.las+xml"

        /// "application/vnd.llamagraphics.life-balance.desktop": Life Balance - Desktop Edition
        let vnd_llamagraphics_life_balance_desktop = ContentType "application/vnd.llamagraphics.life-balance.desktop"

        /// "application/vnd.llamagraphics.life-balance.exchange+xml": Life Balance - Exchange Format
        let vnd_llamagraphics_life_balance_exchange_xml = ContentType "application/vnd.llamagraphics.life-balance.exchange+xml"

        /// "application/vnd.lotus-1-2-3": Lotus 1-2-3
        let vnd_lotus_1_2_3 = ContentType "application/vnd.lotus-1-2-3"

        /// "application/vnd.lotus-approach": Lotus Approach
        let vnd_lotus_approach = ContentType "application/vnd.lotus-approach"

        /// "application/vnd.lotus-freelance": Lotus Freelance
        let vnd_lotus_freelance = ContentType "application/vnd.lotus-freelance"

        /// "application/vnd.lotus-notes": Lotus Notes
        let vnd_lotus_notes = ContentType "application/vnd.lotus-notes"

        /// "application/vnd.lotus-organizer": Lotus Organizer
        let vnd_lotus_organizer = ContentType "application/vnd.lotus-organizer"

        /// "application/vnd.lotus-screencam": Lotus Screencam
        let vnd_lotus_screencam = ContentType "application/vnd.lotus-screencam"

        /// "application/vnd.lotus-wordpro": Lotus Wordpro
        let vnd_lotus_wordpro = ContentType "application/vnd.lotus-wordpro"

        /// "application/vnd.macports.portpkg": MacPorts Port System
        let vnd_macports_portpkg = ContentType "application/vnd.macports.portpkg"

        /// "application/vnd.mcd": Micro CADAM Helix D&D
        let vnd_mcd = ContentType "application/vnd.mcd"

        /// "application/vnd.medcalcdata": MedCalc
        let vnd_medcalcdata = ContentType "application/vnd.medcalcdata"

        /// "application/vnd.mediastation.cdkey": MediaRemote
        let vnd_mediastation_cdkey = ContentType "application/vnd.mediastation.cdkey"

        /// "application/vnd.mfer": Medical Waveform Encoding Format
        let vnd_mfer = ContentType "application/vnd.mfer"

        /// "application/vnd.mfmp": Melody Format for Mobile Platform
        let vnd_mfmp = ContentType "application/vnd.mfmp"

        /// "application/vnd.micrografx.flo": Micrografx
        let vnd_micrografx_flo = ContentType "application/vnd.micrografx.flo"

        /// "application/vnd.micrografx.igx": Micrografx iGrafx Professional
        let vnd_micrografx_igx = ContentType "application/vnd.micrografx.igx"

        /// "application/vnd.mif": FrameMaker Interchange Format
        let vnd_mif = ContentType "application/vnd.mif"

        /// "application/vnd.mobius.daf": Mobius Management Systems - UniversalArchive
        let vnd_mobius_daf = ContentType "application/vnd.mobius.daf"

        /// "application/vnd.mobius.dis": Mobius Management Systems - Distribution Database
        let vnd_mobius_dis = ContentType "application/vnd.mobius.dis"

        /// "application/vnd.mobius.mbk": Mobius Management Systems - Basket file
        let vnd_mobius_mbk = ContentType "application/vnd.mobius.mbk"

        /// "application/vnd.mobius.mqy": Mobius Management Systems - Query File
        let vnd_mobius_mqy = ContentType "application/vnd.mobius.mqy"

        /// "application/vnd.mobius.msl": Mobius Management Systems - Script Language
        let vnd_mobius_msl = ContentType "application/vnd.mobius.msl"

        /// "application/vnd.mobius.plc": Mobius Management Systems - Policy Definition Language File
        let vnd_mobius_plc = ContentType "application/vnd.mobius.plc"

        /// "application/vnd.mobius.txf": Mobius Management Systems - Topic Index File
        let vnd_mobius_txf = ContentType "application/vnd.mobius.txf"

        /// "application/vnd.mophun.application": Mophun VM
        let vnd_mophun_application = ContentType "application/vnd.mophun.application"

        /// "application/vnd.mophun.certificate": Mophun Certificate
        let vnd_mophun_certificate = ContentType "application/vnd.mophun.certificate"

        /// "application/vnd.mozilla.xul+xml": XUL - XML User Interface Language
        let vnd_mozilla_xul_xml = ContentType "application/vnd.mozilla.xul+xml"

        /// "application/vnd.ms-artgalry": Microsoft Artgalry
        let vnd_ms_artgalry = ContentType "application/vnd.ms-artgalry"

        /// "application/vnd.ms-cab-compressed": Microsoft Cabinet File
        let vnd_ms_cab_compressed = ContentType "application/vnd.ms-cab-compressed"

        /// "application/vnd.ms-excel": Microsoft Excel
        let vnd_ms_excel = ContentType "application/vnd.ms-excel"

        /// "application/vnd.ms-excel.addin.macroenabled.12": Microsoft Excel - Add-In File
        let vnd_ms_excel_addin_macroenabled_12 = ContentType "application/vnd.ms-excel.addin.macroenabled.12"

        /// "application/vnd.ms-excel.sheet.binary.macroenabled.12": Microsoft Excel - Binary Workbook
        let vnd_ms_excel_sheet_binary_macroenabled_12 = ContentType "application/vnd.ms-excel.sheet.binary.macroenabled.12"

        /// "application/vnd.ms-excel.sheet.macroenabled.12": Microsoft Excel - Macro-Enabled Workbook
        let vnd_ms_excel_sheet_macroenabled_12 = ContentType "application/vnd.ms-excel.sheet.macroenabled.12"

        /// "application/vnd.ms-excel.template.macroenabled.12": Microsoft Excel - Macro-Enabled Template File
        let vnd_ms_excel_template_macroenabled_12 = ContentType "application/vnd.ms-excel.template.macroenabled.12"

        /// "application/vnd.ms-fontobject": Microsoft Embedded OpenType
        let vnd_ms_fontobject = ContentType "application/vnd.ms-fontobject"

        /// "application/vnd.ms-htmlhelp": Microsoft Html Help File
        let vnd_ms_htmlhelp = ContentType "application/vnd.ms-htmlhelp"

        /// "application/vnd.ms-ims": Microsoft Class Server
        let vnd_ms_ims = ContentType "application/vnd.ms-ims"

        /// "application/vnd.ms-lrm": Microsoft Learning Resource Module
        let vnd_ms_lrm = ContentType "application/vnd.ms-lrm"

        /// "application/vnd.ms-officetheme": Microsoft Office System Release Theme
        let vnd_ms_officetheme = ContentType "application/vnd.ms-officetheme"

        /// "application/vnd.ms-pki.seccat": Microsoft Trust UI Provider - Security Catalog
        let vnd_ms_pki_seccat = ContentType "application/vnd.ms-pki.seccat"

        /// "application/vnd.ms-pki.stl": Microsoft Trust UI Provider - Certificate Trust Link
        let vnd_ms_pki_stl = ContentType "application/vnd.ms-pki.stl"

        /// "application/vnd.ms-powerpoint": Microsoft PowerPoint
        let vnd_ms_powerpoint = ContentType "application/vnd.ms-powerpoint"

        /// "application/vnd.ms-powerpoint.addin.macroenabled.12": Microsoft PowerPoint - Add-in file
        let vnd_ms_powerpoint_addin_macroenabled_12 = ContentType "application/vnd.ms-powerpoint.addin.macroenabled.12"

        /// "application/vnd.ms-powerpoint.presentation.macroenabled.12": Microsoft PowerPoint - Macro-Enabled Presentation File
        let vnd_ms_powerpoint_presentation_macroenabled_12 = ContentType "application/vnd.ms-powerpoint.presentation.macroenabled.12"

        /// "application/vnd.ms-powerpoint.slide.macroenabled.12": Microsoft PowerPoint - Macro-Enabled Open XML Slide
        let vnd_ms_powerpoint_slide_macroenabled_12 = ContentType "application/vnd.ms-powerpoint.slide.macroenabled.12"

        /// "application/vnd.ms-powerpoint.slideshow.macroenabled.12": Microsoft PowerPoint - Macro-Enabled Slide Show File
        let vnd_ms_powerpoint_slideshow_macroenabled_12 = ContentType "application/vnd.ms-powerpoint.slideshow.macroenabled.12"

        /// "application/vnd.ms-powerpoint.template.macroenabled.12": Micosoft PowerPoint - Macro-Enabled Template File
        let vnd_ms_powerpoint_template_macroenabled_12 = ContentType "application/vnd.ms-powerpoint.template.macroenabled.12"

        /// "application/vnd.ms-project": Microsoft Project
        let vnd_ms_project = ContentType "application/vnd.ms-project"

        /// "application/vnd.ms-word.document.macroenabled.12": Micosoft Word - Macro-Enabled Document
        let vnd_ms_word_document_macroenabled_12 = ContentType "application/vnd.ms-word.document.macroenabled.12"

        /// "application/vnd.ms-word.template.macroenabled.12": Micosoft Word - Macro-Enabled Template
        let vnd_ms_word_template_macroenabled_12 = ContentType "application/vnd.ms-word.template.macroenabled.12"

        /// "application/vnd.ms-works": Microsoft Works
        let vnd_ms_works = ContentType "application/vnd.ms-works"

        /// "application/vnd.ms-wpl": Microsoft Windows Media Player Playlist
        let vnd_ms_wpl = ContentType "application/vnd.ms-wpl"

        /// "application/vnd.ms-xpsdocument": Microsoft XML Paper Specification
        let vnd_ms_xpsdocument = ContentType "application/vnd.ms-xpsdocument"

        /// "application/vnd.mseq": 3GPP MSEQ File
        let vnd_mseq = ContentType "application/vnd.mseq"

        /// "application/vnd.musician": MUsical Score Interpreted Code Invented  for the ASCII designation of Notation
        let vnd_musician = ContentType "application/vnd.musician"

        /// "application/vnd.muvee.style": Muvee Automatic Video Editing
        let vnd_muvee_style = ContentType "application/vnd.muvee.style"

        /// "application/vnd.neurolanguage.nlu": neuroLanguage
        let vnd_neurolanguage_nlu = ContentType "application/vnd.neurolanguage.nlu"

        /// "application/vnd.noblenet-directory": NobleNet Directory
        let vnd_noblenet_directory = ContentType "application/vnd.noblenet-directory"

        /// "application/vnd.noblenet-sealer": NobleNet Sealer
        let vnd_noblenet_sealer = ContentType "application/vnd.noblenet-sealer"

        /// "application/vnd.noblenet-web": NobleNet Web
        let vnd_noblenet_web = ContentType "application/vnd.noblenet-web"

        /// "application/vnd.nokia.n-gage.data": N-Gage Game Data
        let vnd_nokia_n_gage_data = ContentType "application/vnd.nokia.n-gage.data"

        /// "application/vnd.nokia.n-gage.symbian.install": N-Gage Game Installer
        let vnd_nokia_n_gage_symbian_install = ContentType "application/vnd.nokia.n-gage.symbian.install"

        /// "application/vnd.nokia.radio-preset": Nokia Radio Application - Preset
        let vnd_nokia_radio_preset = ContentType "application/vnd.nokia.radio-preset"

        /// "application/vnd.nokia.radio-presets": Nokia Radio Application - Preset
        let vnd_nokia_radio_presets = ContentType "application/vnd.nokia.radio-presets"

        /// "application/vnd.novadigm.edm": Novadigm's RADIA and EDM products
        let vnd_novadigm_edm = ContentType "application/vnd.novadigm.edm"

        /// "application/vnd.novadigm.edx": Novadigm's RADIA and EDM products
        let vnd_novadigm_edx = ContentType "application/vnd.novadigm.edx"

        /// "application/vnd.novadigm.ext": Novadigm's RADIA and EDM products
        let vnd_novadigm_ext = ContentType "application/vnd.novadigm.ext"

        /// "application/vnd.oasis.opendocument.chart": OpenDocument Chart
        let vnd_oasis_opendocument_chart = ContentType "application/vnd.oasis.opendocument.chart"

        /// "application/vnd.oasis.opendocument.chart-template": OpenDocument Chart Template
        let vnd_oasis_opendocument_chart_template = ContentType "application/vnd.oasis.opendocument.chart-template"

        /// "application/vnd.oasis.opendocument.database": OpenDocument Database
        let vnd_oasis_opendocument_database = ContentType "application/vnd.oasis.opendocument.database"

        /// "application/vnd.oasis.opendocument.formula": OpenDocument Formula
        let vnd_oasis_opendocument_formula = ContentType "application/vnd.oasis.opendocument.formula"

        /// "application/vnd.oasis.opendocument.formula-template": OpenDocument Formula Template
        let vnd_oasis_opendocument_formula_template = ContentType "application/vnd.oasis.opendocument.formula-template"

        /// "application/vnd.oasis.opendocument.graphics": OpenDocument Graphics
        let vnd_oasis_opendocument_graphics = ContentType "application/vnd.oasis.opendocument.graphics"

        /// "application/vnd.oasis.opendocument.graphics-template": OpenDocument Graphics Template
        let vnd_oasis_opendocument_graphics_template = ContentType "application/vnd.oasis.opendocument.graphics-template"

        /// "application/vnd.oasis.opendocument.image": OpenDocument Image
        let vnd_oasis_opendocument_image = ContentType "application/vnd.oasis.opendocument.image"

        /// "application/vnd.oasis.opendocument.image-template": OpenDocument Image Template
        let vnd_oasis_opendocument_image_template = ContentType "application/vnd.oasis.opendocument.image-template"

        /// "application/vnd.oasis.opendocument.presentation": OpenDocument Presentation
        let vnd_oasis_opendocument_presentation = ContentType "application/vnd.oasis.opendocument.presentation"

        /// "application/vnd.oasis.opendocument.presentation-template": OpenDocument Presentation Template
        let vnd_oasis_opendocument_presentation_template = ContentType "application/vnd.oasis.opendocument.presentation-template"

        /// "application/vnd.oasis.opendocument.spreadsheet": OpenDocument Spreadsheet
        let vnd_oasis_opendocument_spreadsheet = ContentType "application/vnd.oasis.opendocument.spreadsheet"

        /// "application/vnd.oasis.opendocument.spreadsheet-template": OpenDocument Spreadsheet Template
        let vnd_oasis_opendocument_spreadsheet_template = ContentType "application/vnd.oasis.opendocument.spreadsheet-template"

        /// "application/vnd.oasis.opendocument.text": OpenDocument Text
        let vnd_oasis_opendocument_text = ContentType "application/vnd.oasis.opendocument.text"

        /// "application/vnd.oasis.opendocument.text-master": OpenDocument Text Master
        let vnd_oasis_opendocument_text_master = ContentType "application/vnd.oasis.opendocument.text-master"

        /// "application/vnd.oasis.opendocument.text-template": OpenDocument Text Template
        let vnd_oasis_opendocument_text_template = ContentType "application/vnd.oasis.opendocument.text-template"

        /// "application/vnd.oasis.opendocument.text-web": Open Document Text Web
        let vnd_oasis_opendocument_text_web = ContentType "application/vnd.oasis.opendocument.text-web"

        /// "application/vnd.olpc-sugar": Sugar Linux Application Bundle
        let vnd_olpc_sugar = ContentType "application/vnd.olpc-sugar"

        /// "application/vnd.oma.dd2+xml": OMA Download Agents
        let vnd_oma_dd2_xml = ContentType "application/vnd.oma.dd2+xml"

        /// "application/vnd.openofficeorg.extension": Open Office Extension
        let vnd_openofficeorg_extension = ContentType "application/vnd.openofficeorg.extension"

        /// "application/vnd.openxmlformats-officedocument.presentationml.presentation": Microsoft Office - OOXML - Presentation
        let vnd_openxmlformats_officedocument_presentationml_presentation = ContentType "application/vnd.openxmlformats-officedocument.presentationml.presentation"

        /// "application/vnd.openxmlformats-officedocument.presentationml.slide": Microsoft Office - OOXML - Presentation (Slide)
        let vnd_openxmlformats_officedocument_presentationml_slide = ContentType "application/vnd.openxmlformats-officedocument.presentationml.slide"

        /// "application/vnd.openxmlformats-officedocument.presentationml.slideshow": Microsoft Office - OOXML - Presentation (Slideshow)
        let vnd_openxmlformats_officedocument_presentationml_slideshow = ContentType "application/vnd.openxmlformats-officedocument.presentationml.slideshow"

        /// "application/vnd.openxmlformats-officedocument.presentationml.template": Microsoft Office - OOXML - Presentation Template
        let vnd_openxmlformats_officedocument_presentationml_template = ContentType "application/vnd.openxmlformats-officedocument.presentationml.template"

        /// "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet": Microsoft Office - OOXML - Spreadsheet
        let vnd_openxmlformats_officedocument_spreadsheetml_sheet = ContentType "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

        /// "application/vnd.openxmlformats-officedocument.spreadsheetml.template": Microsoft Office - OOXML - Spreadsheet Teplate
        let vnd_openxmlformats_officedocument_spreadsheetml_template = ContentType "application/vnd.openxmlformats-officedocument.spreadsheetml.template"

        /// "application/vnd.openxmlformats-officedocument.wordprocessingml.document": Microsoft Office - OOXML - Word Document
        let vnd_openxmlformats_officedocument_wordprocessingml_document = ContentType "application/vnd.openxmlformats-officedocument.wordprocessingml.document"

        /// "application/vnd.openxmlformats-officedocument.wordprocessingml.template": Microsoft Office - OOXML - Word Document Template
        let vnd_openxmlformats_officedocument_wordprocessingml_template = ContentType "application/vnd.openxmlformats-officedocument.wordprocessingml.template"

        /// "application/vnd.osgeo.mapguide.package": MapGuide DBXML
        let vnd_osgeo_mapguide_package = ContentType "application/vnd.osgeo.mapguide.package"

        /// "application/vnd.osgi.dp": OSGi Deployment Package
        let vnd_osgi_dp = ContentType "application/vnd.osgi.dp"

        /// "application/vnd.palm": PalmOS Data
        let vnd_palm = ContentType "application/vnd.palm"

        /// "application/vnd.pawaafile": PawaaFILE
        let vnd_pawaafile = ContentType "application/vnd.pawaafile"

        /// "application/vnd.pg.format": Proprietary P&G Standard Reporting System
        let vnd_pg_format = ContentType "application/vnd.pg.format"

        /// "application/vnd.pg.osasli": Proprietary P&G Standard Reporting System
        let vnd_pg_osasli = ContentType "application/vnd.pg.osasli"

        /// "application/vnd.picsel": Pcsel eFIF File
        let vnd_picsel = ContentType "application/vnd.picsel"

        /// "application/vnd.pmi.widget": Qualcomm's Plaza Mobile Internet
        let vnd_pmi_widget = ContentType "application/vnd.pmi.widget"

        /// "application/vnd.pocketlearn": PocketLearn Viewers
        let vnd_pocketlearn = ContentType "application/vnd.pocketlearn"

        /// "application/vnd.powerbuilder6": PowerBuilder
        let vnd_powerbuilder6 = ContentType "application/vnd.powerbuilder6"

        /// "application/vnd.previewsystems.box": Preview Systems ZipLock/VBox
        let vnd_previewsystems_box = ContentType "application/vnd.previewsystems.box"

        /// "application/vnd.proteus.magazine": EFI Proteus
        let vnd_proteus_magazine = ContentType "application/vnd.proteus.magazine"

        /// "application/vnd.publishare-delta-tree": PubliShare Objects
        let vnd_publishare_delta_tree = ContentType "application/vnd.publishare-delta-tree"

        /// "application/vnd.pvi.ptid1": Princeton Video Image
        let vnd_pvi_ptid1 = ContentType "application/vnd.pvi.ptid1"

        /// "application/vnd.quark.quarkxpress": QuarkXpress
        let vnd_quark_quarkxpress = ContentType "application/vnd.quark.quarkxpress"

        /// "application/vnd.realvnc.bed": RealVNC
        let vnd_realvnc_bed = ContentType "application/vnd.realvnc.bed"

        /// "application/vnd.recordare.musicxml": Recordare Applications
        let vnd_recordare_musicxml = ContentType "application/vnd.recordare.musicxml"

        /// "application/vnd.recordare.musicxml+xml": Recordare Applications
        let vnd_recordare_musicxml_xml = ContentType "application/vnd.recordare.musicxml+xml"

        /// "application/vnd.rig.cryptonote": CryptoNote
        let vnd_rig_cryptonote = ContentType "application/vnd.rig.cryptonote"

        /// "application/vnd.rim.cod": Blackberry COD File
        let vnd_rim_cod = ContentType "application/vnd.rim.cod"

        /// "application/vnd.rn-realmedia": RealMedia
        let vnd_rn_realmedia = ContentType "application/vnd.rn-realmedia"

        /// "application/vnd.route66.link66+xml": ROUTE 66 Location Based Services
        let vnd_route66_link66_xml = ContentType "application/vnd.route66.link66+xml"

        /// "application/vnd.sailingtracker.track": SailingTracker
        let vnd_sailingtracker_track = ContentType "application/vnd.sailingtracker.track"

        /// "application/vnd.seemail": SeeMail
        let vnd_seemail = ContentType "application/vnd.seemail"

        /// "application/vnd.sema": Secured eMail
        let vnd_sema = ContentType "application/vnd.sema"

        /// "application/vnd.semd": Secured eMail
        let vnd_semd = ContentType "application/vnd.semd"

        /// "application/vnd.semf": Secured eMail
        let vnd_semf = ContentType "application/vnd.semf"

        /// "application/vnd.shana.informed.formdata": Shana Informed Filler
        let vnd_shana_informed_formdata = ContentType "application/vnd.shana.informed.formdata"

        /// "application/vnd.shana.informed.formtemplate": Shana Informed Filler
        let vnd_shana_informed_formtemplate = ContentType "application/vnd.shana.informed.formtemplate"

        /// "application/vnd.shana.informed.interchange": Shana Informed Filler
        let vnd_shana_informed_interchange = ContentType "application/vnd.shana.informed.interchange"

        /// "application/vnd.shana.informed.package": Shana Informed Filler
        let vnd_shana_informed_package = ContentType "application/vnd.shana.informed.package"

        /// "application/vnd.simtech-mindmapper": SimTech MindMapper
        let vnd_simtech_mindmapper = ContentType "application/vnd.simtech-mindmapper"

        /// "application/vnd.smaf": SMAF File
        let vnd_smaf = ContentType "application/vnd.smaf"

        /// "application/vnd.smart.teacher": SMART Technologies Apps
        let vnd_smart_teacher = ContentType "application/vnd.smart.teacher"

        /// "application/vnd.solent.sdkm+xml": SudokuMagic
        let vnd_solent_sdkm_xml = ContentType "application/vnd.solent.sdkm+xml"

        /// "application/vnd.spotfire.dxp": TIBCO Spotfire
        let vnd_spotfire_dxp = ContentType "application/vnd.spotfire.dxp"

        /// "application/vnd.spotfire.sfs": TIBCO Spotfire
        let vnd_spotfire_sfs = ContentType "application/vnd.spotfire.sfs"

        /// "application/vnd.stardivision.calc": StarOffice - Calc
        let vnd_stardivision_calc = ContentType "application/vnd.stardivision.calc"

        /// "application/vnd.stardivision.draw": StarOffice - Draw
        let vnd_stardivision_draw = ContentType "application/vnd.stardivision.draw"

        /// "application/vnd.stardivision.impress": StarOffice - Impress
        let vnd_stardivision_impress = ContentType "application/vnd.stardivision.impress"

        /// "application/vnd.stardivision.math": StarOffice - Math
        let vnd_stardivision_math = ContentType "application/vnd.stardivision.math"

        /// "application/vnd.stardivision.writer": StarOffice - Writer
        let vnd_stardivision_writer = ContentType "application/vnd.stardivision.writer"

        /// "application/vnd.stardivision.writer-global": StarOffice - Writer  (Global)
        let vnd_stardivision_writer_global = ContentType "application/vnd.stardivision.writer-global"

        /// "application/vnd.stepmania.stepchart": StepMania
        let vnd_stepmania_stepchart = ContentType "application/vnd.stepmania.stepchart"

        /// "application/vnd.sun.xml.calc": OpenOffice - Calc (Spreadsheet)
        let vnd_sun_xml_calc = ContentType "application/vnd.sun.xml.calc"

        /// "application/vnd.sun.xml.calc.template": OpenOffice - Calc Template (Spreadsheet)
        let vnd_sun_xml_calc_template = ContentType "application/vnd.sun.xml.calc.template"

        /// "application/vnd.sun.xml.draw": OpenOffice - Draw (Graphics)
        let vnd_sun_xml_draw = ContentType "application/vnd.sun.xml.draw"

        /// "application/vnd.sun.xml.draw.template": OpenOffice - Draw Template (Graphics)
        let vnd_sun_xml_draw_template = ContentType "application/vnd.sun.xml.draw.template"

        /// "application/vnd.sun.xml.impress": OpenOffice - Impress (Presentation)
        let vnd_sun_xml_impress = ContentType "application/vnd.sun.xml.impress"

        /// "application/vnd.sun.xml.impress.template": OpenOffice - Impress Template (Presentation)
        let vnd_sun_xml_impress_template = ContentType "application/vnd.sun.xml.impress.template"

        /// "application/vnd.sun.xml.math": OpenOffice - Math (Formula)
        let vnd_sun_xml_math = ContentType "application/vnd.sun.xml.math"

        /// "application/vnd.sun.xml.writer": OpenOffice - Writer (Text - HTML)
        let vnd_sun_xml_writer = ContentType "application/vnd.sun.xml.writer"

        /// "application/vnd.sun.xml.writer.global": OpenOffice - Writer (Text - HTML)
        let vnd_sun_xml_writer_global = ContentType "application/vnd.sun.xml.writer.global"

        /// "application/vnd.sun.xml.writer.template": OpenOffice - Writer Template (Text - HTML)
        let vnd_sun_xml_writer_template = ContentType "application/vnd.sun.xml.writer.template"

        /// "application/vnd.sus-calendar": ScheduleUs
        let vnd_sus_calendar = ContentType "application/vnd.sus-calendar"

        /// "application/vnd.svd": SourceView Document
        let vnd_svd = ContentType "application/vnd.svd"

        /// "application/vnd.symbian.install": Symbian Install Package
        let vnd_symbian_install = ContentType "application/vnd.symbian.install"

        /// "application/vnd.syncml+xml": SyncML
        let vnd_syncml_xml = ContentType "application/vnd.syncml+xml"

        /// "application/vnd.syncml.dm+wbxml": SyncML - Device Management
        let vnd_syncml_dm_wbxml = ContentType "application/vnd.syncml.dm+wbxml"

        /// "application/vnd.syncml.dm+xml": SyncML - Device Management
        let vnd_syncml_dm_xml = ContentType "application/vnd.syncml.dm+xml"

        /// "application/vnd.tao.intent-module-archive": Tao Intent
        let vnd_tao_intent_module_archive = ContentType "application/vnd.tao.intent-module-archive"

        /// "application/vnd.tmobile-livetv": MobileTV
        let vnd_tmobile_livetv = ContentType "application/vnd.tmobile-livetv"

        /// "application/vnd.trid.tpt": TRI Systems Config
        let vnd_trid_tpt = ContentType "application/vnd.trid.tpt"

        /// "application/vnd.triscape.mxs": Triscape Map Explorer
        let vnd_triscape_mxs = ContentType "application/vnd.triscape.mxs"

        /// "application/vnd.trueapp": True BASIC
        let vnd_trueapp = ContentType "application/vnd.trueapp"

        /// "application/vnd.ufdl": Universal Forms Description Language
        let vnd_ufdl = ContentType "application/vnd.ufdl"

        /// "application/vnd.uiq.theme": User Interface Quartz - Theme (Symbian)
        let vnd_uiq_theme = ContentType "application/vnd.uiq.theme"

        /// "application/vnd.umajin": UMAJIN
        let vnd_umajin = ContentType "application/vnd.umajin"

        /// "application/vnd.unity": Unity 3d
        let vnd_unity = ContentType "application/vnd.unity"

        /// "application/vnd.uoml+xml": Unique Object Markup Language
        let vnd_uoml_xml = ContentType "application/vnd.uoml+xml"

        /// "application/vnd.vcx": VirtualCatalog
        let vnd_vcx = ContentType "application/vnd.vcx"

        /// "application/vnd.visio": Microsoft Visio
        let vnd_visio = ContentType "application/vnd.visio"

        /// "application/vnd.visionary": Visionary
        let vnd_visionary = ContentType "application/vnd.visionary"

        /// "application/vnd.vsf": Viewport+
        let vnd_vsf = ContentType "application/vnd.vsf"

        /// "application/vnd.wap.wbxml": WAP Binary XML (WBXML)
        let vnd_wap_wbxml = ContentType "application/vnd.wap.wbxml"

        /// "application/vnd.wap.wmlc": Compiled Wireless Markup Language (WMLC)
        let vnd_wap_wmlc = ContentType "application/vnd.wap.wmlc"

        /// "application/vnd.wap.wmlscriptc": WMLScript
        let vnd_wap_wmlscriptc = ContentType "application/vnd.wap.wmlscriptc"

        /// "application/vnd.webturbo": WebTurbo
        let vnd_webturbo = ContentType "application/vnd.webturbo"

        /// "application/vnd.wolfram.player": Mathematica Notebook Player
        let vnd_wolfram_player = ContentType "application/vnd.wolfram.player"

        /// "application/vnd.wordperfect": Wordperfect
        let vnd_wordperfect = ContentType "application/vnd.wordperfect"

        /// "application/vnd.wqd": SundaHus WQ
        let vnd_wqd = ContentType "application/vnd.wqd"

        /// "application/vnd.wt.stf": Worldtalk
        let vnd_wt_stf = ContentType "application/vnd.wt.stf"

        /// "application/vnd.xara": CorelXARA
        let vnd_xara = ContentType "application/vnd.xara"

        /// "application/vnd.xfdl": Extensible Forms Description Language
        let vnd_xfdl = ContentType "application/vnd.xfdl"

        /// "application/vnd.yamaha.hv-dic": HV Voice Dictionary
        let vnd_yamaha_hv_dic = ContentType "application/vnd.yamaha.hv-dic"

        /// "application/vnd.yamaha.hv-script": HV Script
        let vnd_yamaha_hv_script = ContentType "application/vnd.yamaha.hv-script"

        /// "application/vnd.yamaha.hv-voice": HV Voice Parameter
        let vnd_yamaha_hv_voice = ContentType "application/vnd.yamaha.hv-voice"

        /// "application/vnd.yamaha.openscoreformat": Open Score Format
        let vnd_yamaha_openscoreformat = ContentType "application/vnd.yamaha.openscoreformat"

        /// "application/vnd.yamaha.openscoreformat.osfpvg+xml": OSFPVG
        let vnd_yamaha_openscoreformat_osfpvg_xml = ContentType "application/vnd.yamaha.openscoreformat.osfpvg+xml"

        /// "application/vnd.yamaha.smaf-audio": SMAF Audio
        let vnd_yamaha_smaf_audio = ContentType "application/vnd.yamaha.smaf-audio"

        /// "application/vnd.yamaha.smaf-phrase": SMAF Phrase
        let vnd_yamaha_smaf_phrase = ContentType "application/vnd.yamaha.smaf-phrase"

        /// "application/vnd.yellowriver-custom-menu": CustomMenu
        let vnd_yellowriver_custom_menu = ContentType "application/vnd.yellowriver-custom-menu"

        /// "application/vnd.zul": Z.U.L. Geometry
        let vnd_zul = ContentType "application/vnd.zul"

        /// "application/vnd.zzazz.deck+xml": Zzazz Deck
        let vnd_zzazz_deck_xml = ContentType "application/vnd.zzazz.deck+xml"

        /// "application/voicexml+xml": VoiceXML
        let voicexml_xml = ContentType "application/voicexml+xml"

        /// "application/widget": Widget Packaging and XML Configuration
        let widget = ContentType "application/widget"

        /// "application/winhlp": WinHelp
        let winhlp = ContentType "application/winhlp"

        /// "application/wsdl+xml": WSDL - Web Services Description Language
        let wsdl_xml = ContentType "application/wsdl+xml"

        /// "application/wspolicy+xml": Web Services Policy
        let wspolicy_xml = ContentType "application/wspolicy+xml"

        /// "application/x-7z-compressed": 7-Zip
        let x_7z_compressed = ContentType "application/x-7z-compressed"

        /// "application/x-abiword": AbiWord
        let x_abiword = ContentType "application/x-abiword"

        /// "application/x-ace-compressed": Ace Archive
        let x_ace_compressed = ContentType "application/x-ace-compressed"

        /// "application/x-authorware-bin": Adobe (Macropedia) Authorware - Binary File
        let x_authorware_bin = ContentType "application/x-authorware-bin"

        /// "application/x-authorware-map": Adobe (Macropedia) Authorware - Map
        let x_authorware_map = ContentType "application/x-authorware-map"

        /// "application/x-authorware-seg": Adobe (Macropedia) Authorware - Segment File
        let x_authorware_seg = ContentType "application/x-authorware-seg"

        /// "application/x-bcpio": Binary CPIO Archive
        let x_bcpio = ContentType "application/x-bcpio"

        /// "application/x-bittorrent": BitTorrent
        let x_bittorrent = ContentType "application/x-bittorrent"

        /// "application/x-bzip": Bzip Archive
        let x_bzip = ContentType "application/x-bzip"

        /// "application/x-bzip2": Bzip2 Archive
        let x_bzip2 = ContentType "application/x-bzip2"

        /// "application/x-cdlink": Video CD
        let x_cdlink = ContentType "application/x-cdlink"

        /// "application/x-chat": pIRCh
        let x_chat = ContentType "application/x-chat"

        /// "application/x-chess-pgn": Portable Game Notation (Chess Games)
        let x_chess_pgn = ContentType "application/x-chess-pgn"

        /// "application/x-cpio": CPIO Archive
        let x_cpio = ContentType "application/x-cpio"

        /// "application/x-csh": C Shell Script
        let x_csh = ContentType "application/x-csh"

        /// "application/x-debian-package": Debian Package
        let x_debian_package = ContentType "application/x-debian-package"

        /// "application/x-director": Adobe Shockwave Player
        let x_director = ContentType "application/x-director"

        /// "application/x-doom": Doom Video Game
        let x_doom = ContentType "application/x-doom"

        /// "application/x-dtbncx+xml": Navigation Control file for XML (for ePub)
        let x_dtbncx_xml = ContentType "application/x-dtbncx+xml"

        /// "application/x-dtbook+xml": Digital Talking Book
        let x_dtbook_xml = ContentType "application/x-dtbook+xml"

        /// "application/x-dtbresource+xml": Digital Talking Book - Resource File
        let x_dtbresource_xml = ContentType "application/x-dtbresource+xml"

        /// "application/x-dvi": Device Independent File Format (DVI)
        let x_dvi = ContentType "application/x-dvi"

        /// "application/x-font-bdf": Glyph Bitmap Distribution Format
        let x_font_bdf = ContentType "application/x-font-bdf"

        /// "application/x-font-ghostscript": Ghostscript Font
        let x_font_ghostscript = ContentType "application/x-font-ghostscript"

        /// "application/x-font-linux-psf": PSF Fonts
        let x_font_linux_psf = ContentType "application/x-font-linux-psf"

        /// "application/x-font-otf": OpenType Font File
        let x_font_otf = ContentType "application/x-font-otf"

        /// "application/x-font-pcf": Portable Compiled Format
        let x_font_pcf = ContentType "application/x-font-pcf"

        /// "application/x-font-snf": Server Normal Format
        let x_font_snf = ContentType "application/x-font-snf"

        /// "application/x-font-ttf": TrueType Font
        let x_font_ttf = ContentType "application/x-font-ttf"

        /// "application/x-font-type1": PostScript Fonts
        let x_font_type1 = ContentType "application/x-font-type1"

        /// "application/x-font-woff": Web Open Font Format
        let x_font_woff = ContentType "application/x-font-woff"

        /// "application/x-futuresplash": FutureSplash Animator
        let x_futuresplash = ContentType "application/x-futuresplash"

        /// "application/x-gnumeric": Gnumeric
        let x_gnumeric = ContentType "application/x-gnumeric"

        /// "application/x-gtar": GNU Tar Files
        let x_gtar = ContentType "application/x-gtar"

        /// "application/x-hdf": Hierarchical Data Format
        let x_hdf = ContentType "application/x-hdf"

        /// "application/x-java-jnlp-file": Java Network Launching Protocol
        let x_java_jnlp_file = ContentType "application/x-java-jnlp-file"

        /// "application/x-latex": LaTeX
        let x_latex = ContentType "application/x-latex"

        /// "application/x-mobipocket-ebook": Mobipocket
        let x_mobipocket_ebook = ContentType "application/x-mobipocket-ebook"

        /// "application/x-ms-application": Microsoft ClickOnce
        let x_ms_application = ContentType "application/x-ms-application"

        /// "application/x-ms-wmd": Microsoft Windows Media Player Download Package
        let x_ms_wmd = ContentType "application/x-ms-wmd"

        /// "application/x-ms-wmz": Microsoft Windows Media Player Skin Package
        let x_ms_wmz = ContentType "application/x-ms-wmz"

        /// "application/x-ms-xbap": Microsoft XAML Browser Application
        let x_ms_xbap = ContentType "application/x-ms-xbap"

        /// "application/x-msaccess": Microsoft Access
        let x_msaccess = ContentType "application/x-msaccess"

        /// "application/x-msbinder": Microsoft Office Binder
        let x_msbinder = ContentType "application/x-msbinder"

        /// "application/x-mscardfile": Microsoft Information Card
        let x_mscardfile = ContentType "application/x-mscardfile"

        /// "application/x-msclip": Microsoft Clipboard Clip
        let x_msclip = ContentType "application/x-msclip"

        /// "application/x-msdownload": Microsoft Application
        let x_msdownload = ContentType "application/x-msdownload"

        /// "application/x-msmediaview": Microsoft MediaView
        let x_msmediaview = ContentType "application/x-msmediaview"

        /// "application/x-msmetafile": Microsoft Windows Metafile
        let x_msmetafile = ContentType "application/x-msmetafile"

        /// "application/x-msmoney": Microsoft Money
        let x_msmoney = ContentType "application/x-msmoney"

        /// "application/x-mspublisher": Microsoft Publisher
        let x_mspublisher = ContentType "application/x-mspublisher"

        /// "application/x-msschedule": Microsoft Schedule+
        let x_msschedule = ContentType "application/x-msschedule"

        /// "application/x-msterminal": Microsoft Windows Terminal Services
        let x_msterminal = ContentType "application/x-msterminal"

        /// "application/x-mswrite": Microsoft Wordpad
        let x_mswrite = ContentType "application/x-mswrite"

        /// "application/x-netcdf": Network Common Data Form (NetCDF)
        let x_netcdf = ContentType "application/x-netcdf"

        /// "application/x-pkcs12": PKCS #12 - Personal Information Exchange Syntax Standard
        let x_pkcs12 = ContentType "application/x-pkcs12"

        /// "application/x-pkcs7-certificates": PKCS #7 - Cryptographic Message Syntax Standard (Certificates)
        let x_pkcs7_certificates = ContentType "application/x-pkcs7-certificates"

        /// "application/x-pkcs7-certreqresp": PKCS #7 - Cryptographic Message Syntax Standard (Certificate Request Response)
        let x_pkcs7_certreqresp = ContentType "application/x-pkcs7-certreqresp"

        /// "application/x-rar-compressed": RAR Archive
        let x_rar_compressed = ContentType "application/x-rar-compressed"

        /// "application/x-sh": Bourne Shell Script
        let x_sh = ContentType "application/x-sh"

        /// "application/x-shar": Shell Archive
        let x_shar = ContentType "application/x-shar"

        /// "application/x-shockwave-flash": Adobe Flash
        let x_shockwave_flash = ContentType "application/x-shockwave-flash"

        /// "application/x-silverlight-app": Microsoft Silverlight
        let x_silverlight_app = ContentType "application/x-silverlight-app"

        /// "application/x-stuffit": Stuffit Archive
        let x_stuffit = ContentType "application/x-stuffit"

        /// "application/x-stuffitx": Stuffit Archive
        let x_stuffitx = ContentType "application/x-stuffitx"

        /// "application/x-sv4cpio": System V Release 4 CPIO Archive
        let x_sv4cpio = ContentType "application/x-sv4cpio"

        /// "application/x-sv4crc": System V Release 4 CPIO Checksum Data
        let x_sv4crc = ContentType "application/x-sv4crc"

        /// "application/x-tar": Tar File (Tape Archive)
        let x_tar = ContentType "application/x-tar"

        /// "application/x-tcl": Tcl Script
        let x_tcl = ContentType "application/x-tcl"

        /// "application/x-tex": TeX
        let x_tex = ContentType "application/x-tex"

        /// "application/x-tex-tfm": TeX Font Metric
        let x_tex_tfm = ContentType "application/x-tex-tfm"

        /// "application/x-texinfo": GNU Texinfo Document
        let x_texinfo = ContentType "application/x-texinfo"

        /// "application/x-ustar": Ustar (Uniform Standard Tape Archive)
        let x_ustar = ContentType "application/x-ustar"

        /// "application/x-wais-source": WAIS Source
        let x_wais_source = ContentType "application/x-wais-source"

        /// "application/x-x509-ca-cert": X.509 Certificate
        let x_x509_ca_cert = ContentType "application/x-x509-ca-cert"

        /// "application/x-xfig": Xfig
        let x_xfig = ContentType "application/x-xfig"

        /// "application/x-xpinstall": XPInstall - Mozilla
        let x_xpinstall = ContentType "application/x-xpinstall"

        /// "application/xcap-diff+xml": XML Configuration Access Protocol - XCAP Diff
        let xcap_diff_xml = ContentType "application/xcap-diff+xml"

        /// "application/xenc+xml": XML Encryption Syntax and Processing
        let xenc_xml = ContentType "application/xenc+xml"

        /// "application/xhtml+xml": XHTML - The Extensible HyperText Markup Language
        let xhtml_xml = ContentType "application/xhtml+xml"

        /// "application/xml": XML - Extensible Markup Language
        let xml = ContentType "application/xml"

        /// "application/xml-dtd": Document Type Definition
        let xml_dtd = ContentType "application/xml-dtd"

        /// "application/xop+xml": XML-Binary Optimized Packaging
        let xop_xml = ContentType "application/xop+xml"

        /// "application/xslt+xml": XML Transformations
        let xslt_xml = ContentType "application/xslt+xml"

        /// "application/xspf+xml": XSPF - XML Shareable Playlist Format
        let xspf_xml = ContentType "application/xspf+xml"

        /// "application/xv+xml": MXML
        let xv_xml = ContentType "application/xv+xml"

        /// "application/yang": YANG Data Modeling Language
        let yang = ContentType "application/yang"

        /// "application/yin+xml": YIN (YANG - XML)
        let yin_xml = ContentType "application/yin+xml"

        /// "application/zip": Zip Archive
        let zip = ContentType "application/zip"

    module Audio =
        /// "audio/adpcm": Adaptive differential pulse-code modulation
        let adpcm = ContentType "audio/adpcm"

        /// "audio/basic": Sun Audio - Au file format
        let basic = ContentType "audio/basic"

        /// "audio/midi": MIDI - Musical Instrument Digital Interface
        let midi = ContentType "audio/midi"

        /// "audio/mp4": MPEG-4 Audio
        let mp4 = ContentType "audio/mp4"

        /// "audio/mpeg": MPEG Audio
        let mpeg = ContentType "audio/mpeg"

        /// "audio/ogg": Ogg Audio
        let ogg = ContentType "audio/ogg"

        /// "audio/vnd.dece.audio": DECE Audio
        let vnd_dece_audio = ContentType "audio/vnd.dece.audio"

        /// "audio/vnd.digital-winds": Digital Winds Music
        let vnd_digital_winds = ContentType "audio/vnd.digital-winds"

        /// "audio/vnd.dra": DRA Audio
        let vnd_dra = ContentType "audio/vnd.dra"

        /// "audio/vnd.dts": DTS Audio
        let vnd_dts = ContentType "audio/vnd.dts"

        /// "audio/vnd.dts.hd": DTS High Definition Audio
        let vnd_dts_hd = ContentType "audio/vnd.dts.hd"

        /// "audio/vnd.lucent.voice": Lucent Voice
        let vnd_lucent_voice = ContentType "audio/vnd.lucent.voice"

        /// "audio/vnd.ms-playready.media.pya": Microsoft PlayReady Ecosystem
        let vnd_ms_playready_media_pya = ContentType "audio/vnd.ms-playready.media.pya"

        /// "audio/vnd.nuera.ecelp4800": Nuera ECELP 4800
        let vnd_nuera_ecelp4800 = ContentType "audio/vnd.nuera.ecelp4800"

        /// "audio/vnd.nuera.ecelp7470": Nuera ECELP 7470
        let vnd_nuera_ecelp7470 = ContentType "audio/vnd.nuera.ecelp7470"

        /// "audio/vnd.nuera.ecelp9600": Nuera ECELP 9600
        let vnd_nuera_ecelp9600 = ContentType "audio/vnd.nuera.ecelp9600"

        /// "audio/vnd.rip": Hit'n'Mix
        let vnd_rip = ContentType "audio/vnd.rip"

        /// "audio/webm": Open Web Media Project - Audio
        let webm = ContentType "audio/webm"

        /// "audio/x-aac": Advanced Audio Coding (AAC)
        let x_aac = ContentType "audio/x-aac"

        /// "audio/x-aiff": Audio Interchange File Format
        let x_aiff = ContentType "audio/x-aiff"

        /// "audio/x-mpegurl": M3U (Multimedia Playlist)
        let x_mpegurl = ContentType "audio/x-mpegurl"

        /// "audio/x-ms-wax": Microsoft Windows Media Audio Redirector
        let x_ms_wax = ContentType "audio/x-ms-wax"

        /// "audio/x-ms-wma": Microsoft Windows Media Audio
        let x_ms_wma = ContentType "audio/x-ms-wma"

        /// "audio/x-pn-realaudio": Real Audio Sound
        let x_pn_realaudio = ContentType "audio/x-pn-realaudio"

        /// "audio/x-pn-realaudio-plugin": Real Audio Sound
        let x_pn_realaudio_plugin = ContentType "audio/x-pn-realaudio-plugin"

        /// "audio/x-wav": Waveform Audio File Format (WAV)
        let x_wav = ContentType "audio/x-wav"

    module Chemical =
        /// "chemical/x-cdx": ChemDraw eXchange file
        let x_cdx = ContentType "chemical/x-cdx"

        /// "chemical/x-cif": Crystallographic Interchange Format
        let x_cif = ContentType "chemical/x-cif"

        /// "chemical/x-cmdf": CrystalMaker Data Format
        let x_cmdf = ContentType "chemical/x-cmdf"

        /// "chemical/x-cml": Chemical Markup Language
        let x_cml = ContentType "chemical/x-cml"

        /// "chemical/x-csml": Chemical Style Markup Language
        let x_csml = ContentType "chemical/x-csml"

        /// "chemical/x-xyz": XYZ File Format
        let x_xyz = ContentType "chemical/x-xyz"

    module Image =
        /// "image/bmp": Bitmap Image File
        let bmp = ContentType "image/bmp"

        /// "image/cgm": Computer Graphics Metafile
        let cgm = ContentType "image/cgm"

        /// "image/g3fax": G3 Fax Image
        let g3fax = ContentType "image/g3fax"

        /// "image/gif": Graphics Interchange Format
        let gif = ContentType "image/gif"

        /// "image/ief": Image Exchange Format
        let ief = ContentType "image/ief"

        /// "image/jpeg": JPEG Image
        let jpeg = ContentType "image/jpeg"

        /// "image/ktx": OpenGL Textures (KTX)
        let ktx = ContentType "image/ktx"

        /// "image/png": Portable Network Graphics (PNG)
        let png = ContentType "image/png"

        /// "image/prs.btif": BTIF
        let prs_btif = ContentType "image/prs.btif"

        /// "image/svg+xml": Scalable Vector Graphics (SVG)
        let svg_xml = ContentType "image/svg+xml"

        /// "image/tiff": Tagged Image File Format
        let tiff = ContentType "image/tiff"

        /// "image/vnd.adobe.photoshop": Photoshop Document
        let vnd_adobe_photoshop = ContentType "image/vnd.adobe.photoshop"

        /// "image/vnd.dece.graphic": DECE Graphic
        let vnd_dece_graphic = ContentType "image/vnd.dece.graphic"

        /// "image/vnd.dvb.subtitle": Close Captioning - Subtitle
        let vnd_dvb_subtitle = ContentType "image/vnd.dvb.subtitle"

        /// "image/vnd.djvu": DjVu
        let vnd_djvu = ContentType "image/vnd.djvu"

        /// "image/vnd.dwg": DWG Drawing
        let vnd_dwg = ContentType "image/vnd.dwg"

        /// "image/vnd.dxf": AutoCAD DXF
        let vnd_dxf = ContentType "image/vnd.dxf"

        /// "image/vnd.fastbidsheet": FastBid Sheet
        let vnd_fastbidsheet = ContentType "image/vnd.fastbidsheet"

        /// "image/vnd.fpx": FlashPix
        let vnd_fpx = ContentType "image/vnd.fpx"

        /// "image/vnd.fst": FAST Search & Transfer ASA
        let vnd_fst = ContentType "image/vnd.fst"

        /// "image/vnd.fujixerox.edmics-mmr": EDMICS 2000
        let vnd_fujixerox_edmics_mmr = ContentType "image/vnd.fujixerox.edmics-mmr"

        /// "image/vnd.fujixerox.edmics-rlc": EDMICS 2000
        let vnd_fujixerox_edmics_rlc = ContentType "image/vnd.fujixerox.edmics-rlc"

        /// "image/vnd.ms-modi": Microsoft Document Imaging Format
        let vnd_ms_modi = ContentType "image/vnd.ms-modi"

        /// "image/vnd.net-fpx": FlashPix
        let vnd_net_fpx = ContentType "image/vnd.net-fpx"

        /// "image/vnd.wap.wbmp": WAP Bitamp (WBMP)
        let vnd_wap_wbmp = ContentType "image/vnd.wap.wbmp"

        /// "image/vnd.xiff": eXtended Image File Format (XIFF)
        let vnd_xiff = ContentType "image/vnd.xiff"

        /// "image/webp": WebP Image
        let webp = ContentType "image/webp"

        /// "image/x-cmu-raster": CMU Image
        let x_cmu_raster = ContentType "image/x-cmu-raster"

        /// "image/x-cmx": Corel Metafile Exchange (CMX)
        let x_cmx = ContentType "image/x-cmx"

        /// "image/x-freehand": FreeHand MX
        let x_freehand = ContentType "image/x-freehand"

        /// "image/x-icon": Icon Image
        let x_icon = ContentType "image/x-icon"

        /// "image/x-pcx": PCX Image
        let x_pcx = ContentType "image/x-pcx"

        /// "image/x-pict": PICT Image
        let x_pict = ContentType "image/x-pict"

        /// "image/x-portable-anymap": Portable Anymap Image
        let x_portable_anymap = ContentType "image/x-portable-anymap"

        /// "image/x-portable-bitmap": Portable Bitmap Format
        let x_portable_bitmap = ContentType "image/x-portable-bitmap"

        /// "image/x-portable-graymap": Portable Graymap Format
        let x_portable_graymap = ContentType "image/x-portable-graymap"

        /// "image/x-portable-pixmap": Portable Pixmap Format
        let x_portable_pixmap = ContentType "image/x-portable-pixmap"

        /// "image/x-rgb": Silicon Graphics RGB Bitmap
        let x_rgb = ContentType "image/x-rgb"

        /// "image/x-xbitmap": X BitMap
        let x_xbitmap = ContentType "image/x-xbitmap"

        /// "image/x-xpixmap": X PixMap
        let x_xpixmap = ContentType "image/x-xpixmap"

        /// "image/x-xwindowdump": X Window Dump
        let x_xwindowdump = ContentType "image/x-xwindowdump"

    module Message =
        /// "message/rfc822": Email Message
        let rfc822 = ContentType "message/rfc822"

    module Model =
        /// "model/iges": Initial Graphics Exchange Specification (IGES)
        let iges = ContentType "model/iges"

        /// "model/mesh": Mesh Data Type
        let mesh = ContentType "model/mesh"

        /// "model/vnd.collada+xml": COLLADA
        let vnd_collada_xml = ContentType "model/vnd.collada+xml"

        /// "model/vnd.dwf": Autodesk Design Web Format (DWF)
        let vnd_dwf = ContentType "model/vnd.dwf"

        /// "model/vnd.gdl": Geometric Description Language (GDL)
        let vnd_gdl = ContentType "model/vnd.gdl"

        /// "model/vnd.gtw": Gen-Trix Studio
        let vnd_gtw = ContentType "model/vnd.gtw"

        /// "model/vnd.mts": Virtue MTS
        let vnd_mts = ContentType "model/vnd.mts"

        /// "model/vnd.vtu": Virtue VTU
        let vnd_vtu = ContentType "model/vnd.vtu"

        /// "model/vrml": Virtual Reality Modeling Language
        let vrml = ContentType "model/vrml"

    module Text =
        /// "text/calendar": iCalendar
        let calendar = ContentType "text/calendar"

        /// "text/css": Cascading Style Sheets (CSS)
        let css = ContentType "text/css"

        /// "text/csv": Comma-Seperated Values
        let csv = ContentType "text/csv"

        /// "text/html": HyperText Markup Language (HTML)
        let html = ContentType "text/html"

        /// "text/n3": Notation3
        let n3 = ContentType "text/n3"

        /// "text/plain": Text File
        let plain = ContentType "text/plain"

        /// "text/prs.lines.tag": PRS Lines Tag
        let prs_lines_tag = ContentType "text/prs.lines.tag"

        /// "text/richtext": Rich Text Format (RTF)
        let richtext = ContentType "text/richtext"

        /// "text/sgml": Standard Generalized Markup Language (SGML)
        let sgml = ContentType "text/sgml"

        /// "text/tab-separated-values": Tab Seperated Values
        let tab_separated_values = ContentType "text/tab-separated-values"

        /// "text/troff": troff
        let troff = ContentType "text/troff"

        /// "text/turtle": Turtle (Terse RDF Triple Language)
        let turtle = ContentType "text/turtle"

        /// "text/uri-list": URI Resolution Services
        let uri_list = ContentType "text/uri-list"

        /// "text/vnd.curl": Curl - Applet
        let vnd_curl = ContentType "text/vnd.curl"

        /// "text/vnd.curl.dcurl": Curl - Detached Applet
        let vnd_curl_dcurl = ContentType "text/vnd.curl.dcurl"

        /// "text/vnd.curl.scurl": Curl - Source Code
        let vnd_curl_scurl = ContentType "text/vnd.curl.scurl"

        /// "text/vnd.curl.mcurl": Curl - Manifest File
        let vnd_curl_mcurl = ContentType "text/vnd.curl.mcurl"

        /// "text/vnd.fly": mod_fly / fly.cgi
        let vnd_fly = ContentType "text/vnd.fly"

        /// "text/vnd.fmi.flexstor": FLEXSTOR
        let vnd_fmi_flexstor = ContentType "text/vnd.fmi.flexstor"

        /// "text/vnd.graphviz": Graphviz
        let vnd_graphviz = ContentType "text/vnd.graphviz"

        /// "text/vnd.in3d.3dml": In3D - 3DML
        let vnd_in3d_3dml = ContentType "text/vnd.in3d.3dml"

        /// "text/vnd.in3d.spot": In3D - 3DML
        let vnd_in3d_spot = ContentType "text/vnd.in3d.spot"

        /// "text/vnd.sun.j2me.app-descriptor": J2ME App Descriptor
        let vnd_sun_j2me_app_descriptor = ContentType "text/vnd.sun.j2me.app-descriptor"

        /// "text/vnd.wap.wml": Wireless Markup Language (WML)
        let vnd_wap_wml = ContentType "text/vnd.wap.wml"

        /// "text/vnd.wap.wmlscript": Wireless Markup Language Script (WMLScript)
        let vnd_wap_wmlscript = ContentType "text/vnd.wap.wmlscript"

        /// "text/x-asm": Assembler Source File
        let x_asm = ContentType "text/x-asm"

        /// "text/x-c": C Source File
        let x_c = ContentType "text/x-c"

        /// "text/x-fortran": Fortran Source File
        let x_fortran = ContentType "text/x-fortran"

        /// "text/x-pascal": Pascal Source File
        let x_pascal = ContentType "text/x-pascal"

        /// "text/x-java-source,java": Java Source File
        let x_java_source_java = ContentType "text/x-java-source,java"

        /// "text/x-setext": Setext
        let x_setext = ContentType "text/x-setext"

        /// "text/x-uuencode": UUEncode
        let x_uuencode = ContentType "text/x-uuencode"

        /// "text/x-vcalendar": vCalendar
        let x_vcalendar = ContentType "text/x-vcalendar"

        /// "text/x-vcard": vCard
        let x_vcard = ContentType "text/x-vcard"

        /// "text/plain-bas": BAS Partitur Format
        let plain_bas = ContentType "text/plain-bas"

    module Video =
        /// "video/3gpp": 3GP
        let _3gpp = ContentType "video/3gpp"

        /// "video/3gpp2": 3GP2
        let _3gpp2 = ContentType "video/3gpp2"

        /// "video/h261": H.261
        let h261 = ContentType "video/h261"

        /// "video/h263": H.263
        let h263 = ContentType "video/h263"

        /// "video/h264": H.264
        let h264 = ContentType "video/h264"

        /// "video/jpeg": JPGVideo
        let jpeg = ContentType "video/jpeg"

        /// "video/jpm": JPEG 2000 Compound Image File Format
        let jpm = ContentType "video/jpm"

        /// "video/mj2": Motion JPEG 2000
        let mj2 = ContentType "video/mj2"

        /// "video/mp4": MPEG-4 Video
        let mp4 = ContentType "video/mp4"

        /// "video/mpeg": MPEG Video
        let mpeg = ContentType "video/mpeg"

        /// "video/ogg": Ogg Video
        let ogg = ContentType "video/ogg"

        /// "video/quicktime": Quicktime Video
        let quicktime = ContentType "video/quicktime"

        /// "video/vnd.dece.hd": DECE High Definition Video
        let vnd_dece_hd = ContentType "video/vnd.dece.hd"

        /// "video/vnd.dece.mobile": DECE Mobile Video
        let vnd_dece_mobile = ContentType "video/vnd.dece.mobile"

        /// "video/vnd.dece.pd": DECE PD Video
        let vnd_dece_pd = ContentType "video/vnd.dece.pd"

        /// "video/vnd.dece.sd": DECE SD Video
        let vnd_dece_sd = ContentType "video/vnd.dece.sd"

        /// "video/vnd.dece.video": DECE Video
        let vnd_dece_video = ContentType "video/vnd.dece.video"

        /// "video/vnd.fvt": FAST Search & Transfer ASA
        let vnd_fvt = ContentType "video/vnd.fvt"

        /// "video/vnd.mpegurl": MPEG Url
        let vnd_mpegurl = ContentType "video/vnd.mpegurl"

        /// "video/vnd.ms-playready.media.pyv": Microsoft PlayReady Ecosystem Video
        let vnd_ms_playready_media_pyv = ContentType "video/vnd.ms-playready.media.pyv"

        /// "video/vnd.uvvu.mp4": DECE MP4
        let vnd_uvvu_mp4 = ContentType "video/vnd.uvvu.mp4"

        /// "video/vnd.vivo": Vivo
        let vnd_vivo = ContentType "video/vnd.vivo"

        /// "video/webm": Open Web Media Project - Video
        let webm = ContentType "video/webm"

        /// "video/x-f4v": Flash Video
        let x_f4v = ContentType "video/x-f4v"

        /// "video/x-fli": FLI/FLC Animation Format
        let x_fli = ContentType "video/x-fli"

        /// "video/x-flv": Flash Video
        let x_flv = ContentType "video/x-flv"

        /// "video/x-m4v": M4v
        let x_m4v = ContentType "video/x-m4v"

        /// "video/x-ms-asf": Microsoft Advanced Systems Format (ASF)
        let x_ms_asf = ContentType "video/x-ms-asf"

        /// "video/x-ms-wm": Microsoft Windows Media
        let x_ms_wm = ContentType "video/x-ms-wm"

        /// "video/x-ms-wmv": Microsoft Windows Media Video
        let x_ms_wmv = ContentType "video/x-ms-wmv"

        /// "video/x-ms-wmx": Microsoft Windows Media Audio/Video Playlist
        let x_ms_wmx = ContentType "video/x-ms-wmx"

        /// "video/x-ms-wvx": Microsoft Windows Media Video Playlist
        let x_ms_wvx = ContentType "video/x-ms-wvx"

        /// "video/x-msvideo": Audio Video Interleave (AVI)
        let x_msvideo = ContentType "video/x-msvideo"

        /// "video/x-sgi-movie": SGI Movie
        let x_sgi_movie = ContentType "video/x-sgi-movie"

    module X_conference =
        /// "x-conference/x-cooltalk": CoolTalk
        let x_cooltalk = ContentType "x-conference/x-cooltalk"
