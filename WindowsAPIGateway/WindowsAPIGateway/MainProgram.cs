using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
