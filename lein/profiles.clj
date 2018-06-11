{:init {:aether #=(eval (do (require 'cemerick.pomegranate.aether)
                            (cemerick.pomegranate.aether/register-wagon-factory!
                             "http" #(org.apache.maven.wagon.providers.http.HttpWagon.))))}
 :user {:plugin-repositories [["mail-release" {:url "http://maven.daumcorp.com/content/repositories/dk-mailcloud-release"
                                               :update :always}]
                              ["mail-snapshot" {:url "http://maven.daumcorp.com/content/repositories/dk-mailcloud-snapshots"
                                                :update :always}]]
        :plugins [[lein-try "0.4.3"]]}}
