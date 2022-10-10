{:user  {:java-cmd "/usr/lib/jvm/java-17-openjdk-amd64/bin/java"
         :jvm-opts ^:replace ["-Xmx3G"]
         :plugins [
                   ;; Convenience
                   [lein-ancient "0.6.15"]
                   [lein-pprint "1.2.0"]
]
         }}
