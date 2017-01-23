using System;

namespace WindowsAPIGateway
{
    class MainProgram
    {
        static void Main(string[] args)
        {
            string desiredProgram = String.Join(" ", args);

            SetCurrentProgram.ActivateApp(desiredProgram);
        }
    }
}
