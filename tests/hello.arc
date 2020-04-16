#test basic_main
#source
    def main := () -> {
        (import std::io)::print("Hello world");
    }
#expected
    Define
        Name
        InferredType
        Function
            List
            InferredType
            Block
                Call
                    Access
                        Import
                            Path
                                Name
                                Name
                        Name
                    List
                        ListMember
                            None
                            None
                            String