{-# LANGUAGE OverloadedStrings #-}
module Data.HBCI.Constants where

import qualified Data.Text as T

secfunc_hbci_sig_rdh :: T.Text
secfunc_hbci_sig_rdh = "1"

secfunc_hbci_sig_ddv :: T.Text
secfunc_hbci_sig_ddv = "2"

secfunc_fints_sig_dig :: T.Text
secfunc_fints_sig_dig = "1"

secfunc_fints_sig_sig :: T.Text
secfunc_fints_sig_sig = "2"


secfunc_sig_pt_1step :: T.Text
secfunc_sig_pt_1step = "999"

secfunc_sig_pt_2step_min :: T.Text
secfunc_sig_pt_2step_min = "900"

secfunc_sig_pt_2step_max :: T.Text
secfunc_sig_pt_2step_max = "997"


hashalg_sha1 :: T.Text
hashalg_sha1 = "1"

hashalg_sha256 :: T.Text
hashalg_sha256 = "3"

hashalg_sha384 :: T.Text
hashalg_sha384 = "4"

hashalg_sha512 :: T.Text
hashalg_sha512 = "5"

hashalg_sha256_sha256 :: T.Text
hashalg_sha256_sha256 = "6"

hashalg_ripemd160 :: T.Text
hashalg_ripemd160 = "999"


sigalg_des :: T.Text
sigalg_des = "1"

sigalg_rsa :: T.Text
sigalg_rsa = "10"


sigmode_iso9796_1 :: T.Text
sigmode_iso9796_1 = "16"

sigmode_iso9796_2 :: T.Text
sigmode_iso9796_2 = "17"

sigmode_pkcs1 :: T.Text
sigmode_pkcs1 = "18"

sigmode_pss :: T.Text
sigmode_pss = "19"

sigmode_retail_mac :: T.Text
sigmode_retail_mac = "999"
