class Contract:
    """
    All available contracts here.
    """

    def always_succeed(self, inputs:dict, outputs:dict) -> bool:
        """
        This will always succeed.
        """
        return True

    def always_fails(self, inputs:dict, outputs:dict) -> bool:
        """
        This will always fail.
        """
        return False
