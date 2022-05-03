class Contract:
    """
    All available contracts here.
    """
    
    def always_succeed(self, inputs:dict, outputs:dict) -> bool:
        """
        All simple payments run this contract.
        """
        return True

    def always_fails(self, inputs:dict, outputs:dict) -> bool:
        """
        Testing for auto fail contract.
        """
        return False
